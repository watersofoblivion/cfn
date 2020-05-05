Compiler Source
===

The source code is broken up into multiple sub-libraries along functional lines.  Each sub-library has its own README documenting the specifics of that library.  In roughly dependency order, the libraries are:

* [Syntax](syntax/README.md) - Front-end: Lexing, Parsing, Abstract Syntax, Location Tracking, and Pretty-Printing
* [Ext](ext/README.md) - External data, such as the AWS API spec and the CloudFormation spec, used to codegen the standard library
* [Ir](ir/README.md) - Type checking and conversion to Intermediate Representation in ANF
* [Opt](opt/README.md) - Optimizer
* [Codegen](codegen/README.md) - Closure conversion, AWS Lambda runtime, and generation of low-level bytecode using LLVM.
* [Build](build/README.md) - Dependency management, unified build process.
* [Cli](cli/README.md) - Command-line interface
