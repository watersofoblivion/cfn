CFN++
===

CFN++ is a native-code compiler for AWS::Serverless.

* [Install](#install)
* [Crash Course](#crash-course)

Install
===

Currently, the only way to install CFN++ is to build from source.  The build will produce a `cfn++` command-line tool a la the `go` tool.

Dependencies
---

The CFN++ compiler is written in OCaml and builds code with LLVM.  You can install the necessary toolchain with Homebrew on OSX:

```bash
# Install OCaml and the build tool
brew install ocaml dune

# Install LLVM (after OCaml, so that the OCaml bindings are generated)
brew install llvm
```

CFN++
---

To build CFN++, download the source and build with Dune:

```bash
# Download the source code
git clone https://github.com/watersofoblivion/cfn
cd cfn++

# Recommended: Run tests
dune runtest

# Build
dune build
```

If all goes well, the command-line tool will be located at `_build/install/default/bin/cfn++` relative to the source directory.  Run `cfn++ help` for a full listing of command-line options.

Crash Course
===

TODO
