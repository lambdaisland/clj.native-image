# lambdaisland/native-image

<!-- badges -->
<p align=center>
[![cljdoc badge](https://cljdoc.org/badge/com.lambdaisland/native-image)](https://cljdoc.org/d/com.lambdaisland/native-image) [![Clojars Project](https://img.shields.io/clojars/v/com.lambdaisland/native-image.svg)](https://clojars.org/com.lambdaisland/native-image) ![](https://img.shields.io/clojars/dt/com.lambdaisland%2Fnative-image?style=flat-square)
</p>
<!-- /badges -->

Modernised fork of
[taylorwood/clj.native-image](https://github.com/taylorwood/clj.native-image),
with many thanks to the original author.

Build [GraalVM](https://www.graalvm.org) native images using [Clojure Deps and CLI tools](https://clojure.org/guides/deps_and_cli).

Useful for creating lightweight, native CLI executables using Clojure and `deps.edn`.

It's operation is fairly simple, it invokes Clojure's compiler to turn
namespaces into Java class files, it then invokes the GraalVM `native-image`
tool to create a single binary.

## Prerequisites

- [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
- [GraalVM](https://www.graalvm.org/downloads/)

Recent enough GraalVM distributions have `native-image` directly under `bin/`.
If either `PATH` or `JAVA_HOME` is set correctly then it'll be found
automatically.

Tested with GraalVM CE 21 and 25.

<!-- installation -->
## Installation

To use the latest release, add the following to your `deps.edn` ([Clojure CLI](https://clojure.org/guides/deps_and_cli))

```clj
com.lambdaisland/native-image {:mvn/version "0.3.58"}
```

or add the following to your `project.clj` ([Leiningen](https://leiningen.org/))

```clj
[com.lambdaisland/native-image "0.3.58"]
```
<!-- /installation -->

## Usage

The main namespace is `lambdaisland.native-image`, which has a single subcommand, `build`

```
$ clojure -M -m lambdaisland.native-image build --help
NAME
  clojure -M -m lambdaisland.native-image build  ——  Build a native image, with `main-ns` as entry point.

SYNOPSIS
  clojure -M -m lambdaisland.native-image build <main-ns> [-n | --native-image-path <path>] [-e | --echo]
    [-p | --precompile <namespace>] [--compile-path <path>] [<args>...]

FLAGS
  -n, --native-image-path <path>   Use a specific native-image binary. (default "/home/arne/opt/graalvm-community-25.3.4.1+1.1/bin/native-image")
  -e, --echo                       Print out native-image invocation
  -p, --precompile <namespace>     Namespace to compile before the main ns, e.g. because they contain gen-class directives
      --compile-path <path>        Clojure's compilation output path (default "target")
```

Pass it a namespace to use as its main entrypoint. It should have `(:gen-class)`
and a `main` function. Additional flags after `--` are passed on directly to
`native-image`.

For example, to build itself:

```
clojure -M -m lambdaisland.native-image build lambdaisland.native-image -- --initialize-at-build-time -march=native
```

<!-- opencollective -->
## Lambda Island Open Source

Thank you! native-image is made possible thanks to our generous backers. [Become a
backer on OpenCollective](https://opencollective.com/lambda-island) so that we
can continue to make native-image better.

<a href="https://opencollective.com/lambda-island">
<img src="https://opencollective.com/lambda-island/organizations.svg?avatarHeight=46&width=800&button=false">
<img src="https://opencollective.com/lambda-island/individuals.svg?avatarHeight=46&width=800&button=false">
</a>
<img align="left" src="https://github.com/lambdaisland/open-source/raw/master/artwork/lighthouse_readme.png">

&nbsp;

native-image is part of a growing collection of quality Clojure libraries created and maintained
by the fine folks at [Gaiwan](https://gaiwan.co).

Pay it forward by [becoming a backer on our OpenCollective](http://opencollective.com/lambda-island),
so that we continue to enjoy a thriving Clojure ecosystem.

You can find an overview of all our different projects at [lambdaisland/open-source](https://github.com/lambdaisland/open-source).

&nbsp;

&nbsp;
<!-- /opencollective -->


<!-- license -->
## License

Copyright &copy; 2018-2026 Taylor Wood, Arne Brasseur, and Contributors

Licensed under the term of the MIT License, see LICENSE.
<!-- /license -->