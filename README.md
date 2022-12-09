CFN++
===

CFN++ is a native-code compiler for AWS::Serverless.

* [Install](#install)
* [Quickstart](#quickstart)
* [Developing](#developing)

Install
===

Currently, the only way to install CFN++ is to build from source.  The compiler is written in OCaml and uses LLVM to generate code.  A successful build will produce a `cfn++` command-line tool a la the `go` tool.

External Dependencies
---

First, install the core dependencies needed to build the compiler itself: the [OCaml](https://ocaml.org/) compiler and the [Opam](https://opam.ocaml.org/) package manager.

```bash
brew install ocaml opam
sudo apt install ocaml opam
```

CFN++ generates code with [LLVM](https://llvm.org).  This needs to be installed after OCaml so that the OCaml language bindings are generated correctly.

```bash
brew install llvm
sudo apt install llvm
```

Next, initialize Opam and bring it up to date.

```bash
opam init
opam update
```

Clone the CFN++ source from [GitHub](https://github.com/watersofoblivion/cfn):

```bash
git clone https://github.com/watersofoblivion/cfn
cd cfn
```

You can then install CFN++ using the `opam` tool.  This will install all of the dependencies and build the compiler.  This will take a little while as Opam builds all dependencies from source.

```bash
opam install .
```

Quickstart
===

TODO

Developing
===

Useful Commands
---

### Opam

* `opam install .` in the project directory installs the dependencies for the project
* `opam update .` in the project directory updates the dependencies for the project.  (Does not process uncomitted changes.)

### Dune

* `dune runtest -w` runs test continuously
