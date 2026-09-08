(ns lambdaisland.native-image
  "Builds GraalVM native images from deps.edn projects."
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.deps.edn :as deps-edn]
   [clojure.tools.namespace.find :refer [find-namespaces-in-dir]]
   [lambdaisland.cli :as cli])
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
  (let [{root-edn :root user-edn :user project-edn :project extra-edn :extra} (deps-edn/create-edn-maps)]
    (deps-edn/merge-edns [root-edn user-edn extra-edn])))

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
                   (seq opts) (into opts)
                   cp         (into ["-cp" cp])
                   main       (conj main))]
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
                     (System/getenv "GRAALVM_HOME")
                     (str (System/getenv "JAVA_HOME") "/bin")]
        paths (lazy-cat graal-paths (str/split (System/getenv "PATH") (re-pattern (File/pathSeparator))))
        filename (cond-> "native-image" windows? (str ".cmd"))]
    (first
     (for [path (distinct paths)
           :let [file (io/file path filename)]
           :when (.exists file)]
       (.getAbsolutePath file)))))

(defn- munge-class-name [class-name]
  (str/replace class-name "-" "_"))

(defn build [main-ns graal-args {:keys [precompile native-image-path compile-path] :as opts}]
  (let [deps-map   (merged-deps)
        namespaces (map symbol (concat precompile ["clojure.core" main-ns]))
        namespaces (concat namespaces
                           (->> (:paths deps-map)
                                (mapcat (comp find-namespaces-in-dir io/file))
                                (remove (set namespaces))))]

    (prep-compile-path)

    (try
      (push-thread-bindings {#'clojure.core/*loaded-libs* (ref (sorted-set))})
      (binding [*compiler-options* {:elide-meta     []
                                    :direct-linking true}
                *compile-path*     compile-path]

        (.mkdirs (io/file *compile-path*))

        (doseq [ns (distinct namespaces)]
          (println "Compiling" ns)
          (compile ns))

        (System/exit
         (exec-native-image
          graal-args
          (native-image-classpath)
          (munge-class-name main-ns)
          opts)))
      (finally
        (pop-thread-bindings)))))

(defn cmd
  "Build a native image, with `main-ns` as entry point."
  [{:keys [native-image-path main-ns]
    :lambdaisland.cli/keys [argv] :as opts}]
  (try
    (cond
      (not native-image-path)
      (binding [*out* *err*]
        (println "Could not find GraalVM's native-image! Please make sure that the environment variable $GRAALVM_HOME is set. The native-image tool must also be installed ($GRAALVM_HOME/bin/gu install native-image).")
        (println "If you do not wish to set the GRAALVM_HOME environment variable, you can use the --native-image-path flag to set the binary explicity. Try --help for opts.")
        #_(System/exit 1))

      (not main-ns)
      (binding [*out* *err*]
        (println "Main namespace required e.g. \"script\" if main file is ./script.clj")
        #_(System/exit 1))

      :else
      (build main-ns (next argv) opts))
    (finally
      (shutdown-agents))))

(def flags
  ["-n, --native-image-path <path>" {:doc     "Use a specific native-image binary."
                                     :default (native-image-bin-path)}
   "-e, --echo"                     "Print out native-image invocation"
   "-p, --precompile <namespace>"   {:doc   "Namespace to compile before the main ns, e.g. because they contain gen-class directives"
                                     :coll? true}
   "--compile-path <path>" {:doc "Clojure's compilation output path"
                            :default "target"}])

(defn -main [& args]
  (cli/dispatch
   {:name    "clojure -M -m lambdaisland.native-image"
    :doc     "Build GraalVM native-image binaries from Clojure projects.

If no --native-image-path is provided then it is searched for in $GRAALVM_HOME/bin, $GRAALVM_HOME, $JAVA_HOME/bin and finally the regular $PATH.

Additional arguments after `--` are passed directly to GraalVM native-image."
    :commands ["build <main-ns>" #'cmd]
    :flags   flags}
   args))
