(ns clj.native-image
  "Builds GraalVM native images from deps.edn projects."
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]
   [clojure.tools.deps.alpha :as deps]
   [clojure.tools.namespace.find :refer [find-namespaces-in-dir]])
  (:import
   (java.io BufferedReader File)))

(defn native-image-classpath
  "Returns the current tools.deps classpath string, minus clj.native-image and plus *compile-path*."
  []
  (as-> (System/getProperty "java.class.path") $
    (str/split $ (re-pattern (str File/pathSeparatorChar)))
    (remove #(str/includes? "clj.native-image" %) $) ;; exclude ourselves
    (cons *compile-path* $) ;; prepend compile path for classes
    (str/join File/pathSeparatorChar $)))

(def windows? (str/starts-with? (System/getProperty "os.name") "Windows"))

(defn merged-deps
  "Merges install, user, local deps.edn maps left-to-right."
  []
  (let [{:keys [install-edn user-edn project-edn]} (deps/find-edn-maps)]
    (deps/merge-edns [install-edn user-edn project-edn])))

(defn sh
  "Launches a process with optional args, returning exit code.
  Prints stdout & stderr."
  [bin & args]
  (let [arg-array ^"[Ljava.lang.String;" (into-array String (cons bin args))
        process (-> (ProcessBuilder. arg-array)
                    (.redirectErrorStream true) ;; TODO stream stderr to stderr
                    (.start))]
    (with-open [out (io/reader (.getInputStream process))]
      (loop []
        (when-let [line (.readLine ^BufferedReader out)]
          (println line)
          (recur))))
    (.waitFor process)))

(defn exec-native-image
  "Executes native-image (bin) with opts, specifying a classpath,
   main/entrypoint class, and destination path."
  [opts cp main {:keys [native-image-path echo]}]
  (let [cli-args (cond-> []
                   (seq opts)     (into opts)
                   cp             (into ["-cp" cp])
                   main           (conj main)
                   ;; apparently native-image --no-server isn't currently supported on Windows
                   (not windows?) (conj "--no-server"))]
    (when echo
      (println (str native-image-path " " (str/join " " (map #(if (str/includes? % " ") (str "'" % "'") %) cli-args)))))
    (apply sh native-image-path cli-args)))

(defn prep-compile-path []
  (let [compile-path (io/file *compile-path*)]
    (doseq [file (-> compile-path (file-seq) (rest) (reverse))]
      (io/delete-file file))
    (.mkdir compile-path)))

(defn native-image-bin-path []
  (let [graal-paths [(str (System/getenv "GRAALVM_HOME") "/bin")
                     (System/getenv "GRAALVM_HOME")]
        paths (lazy-cat graal-paths (str/split (System/getenv "PATH") (re-pattern (File/pathSeparator))))
        filename (cond-> "native-image" windows? (str ".cmd"))]
    (first
     (for [path (distinct paths)
           :let [file (io/file path filename)]
           :when (.exists file)]
       (.getAbsolutePath file)))))

(defn- munge-class-name [class-name]
  (str/replace class-name "-" "_"))

(defn build [main-ns graal-args {:keys [precompile native-image-path] :as opts}]
  (let [deps-map (merged-deps)
        namespaces (map symbol (concat
                                (remove empty? (str/split (or precompile "") #","))
                                [main-ns]))
        namespaces (concat namespaces
                           (->> (:paths deps-map)
                                (mapcat (comp find-namespaces-in-dir io/file))
                                (remove (set namespaces))))]
    (prep-compile-path)
    (doseq [ns (distinct namespaces)]
      (println "Compiling" ns)
      (compile ns))

    (System/exit
     (exec-native-image
      graal-args
      (native-image-classpath)
      (munge-class-name main-ns)
      opts))))

(defn- accumulate [m k v]
  (update m k (fnil conj []) v))

(def cli-opts
  [["-n"  "--native-image-path"      "Use a specific native-image binary."
    :default (native-image-bin-path)]
   ["-e" "--echo"                   "Print out native-image invocation"]
   ["-p"  "--precompile NAMESPACES" "Namespaces to compile before the main ns, e.g. because they contain gen-class directives, comma separated."]
   ["-h" "--help"                   "Output this help information."]])

(defn print-help [summary]
  (println "Usage: clojure -m clj.native-image [MAIN_NS] [OPTS] -- [GRAAL_ARGS]")
  (println)
  (println summary)
  (println)
  (println "If no --native-image-path is provided then it is search for in $GRAALVM_HOME/bin, $GRAALVM_HOME, and $PATH.")
  (println)
  (println "Any arguments after -- are passed on verbatim to native-image."))

(defn -main [& args]
  (let [[our-args graal-args] (split-with (complement #{"--"}) args)
        {:keys [options arguments summary]} (cli/parse-opts our-args cli-opts)]
    (try
      (cond
        (:help options)
        (print-help summary)

        (not (:native-image-path options))
        (binding [*out* *err*]
          (println "Could not find GraalVM's native-image! Please make sure that the environment variable $GRAALVM_HOME is set. The native-image tool must also be installed ($GRAALVM_HOME/bin/gu install native-image).")
          (println "If you do not wish to set the GRAALVM_HOME environment variable, you can use the --native-image-path flag to set the binary explicity. Try --help for options.")
          (System/exit 1))

        (not (seq arguments))
        (binding [*out* *err*]
          (println "Main namespace required e.g. \"script\" if main file is ./script.clj")
          (print-help summary)
          (System/exit 1))

        :else
        (build (first arguments) (next graal-args) options))
      (finally
        (shutdown-agents)))))
