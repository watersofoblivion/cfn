Runtime
===

This module generates the Lambda runtime for all defined functions.  The entire
runtime is generated as LLVM bytecode.

### Design

To simplify the use of the generated runtime functions by downstream code
generators, it is convenient to bind the generated functions to OCaml
identifiers partitioned into logical modules.  Since these functions are
dynamic values that are not available until runtime, this is accomplished using
first-class modules.

An OCaml module containing the target LLVM module and context is bound.  Each of
the downstream modules that generate the runtime are parameterized by this type,
generating their code and binding the generated functions to identifiers when
they are applied to the target module.  The values bound by these identifiers
are `LLvalues`, suitable for use in further code generation, not `Ctypes` values
that can be executed from OCaml code.

The runtime is stratified into layers, with each layer encapsulated in a module.
Modules for higher level layers are parameterized by both the lowest-level
target module and modules with bindings to the lower level layers they depend
on.

### Testing

Unit testing of the runtime uses first class modules in a similar fashion as
code generation.

Similar to the base LLVM target module for code generation, there is a base
executable module for unit testing that is parameterized by a LLVM target module
and JIT compiles that module when applied, exposing the resulting LLVM execution
engine and helpers for binding compiled functions.

In parallel with the code generation modules, there are a series of test modules
parameterized by a base executable module, the codegen module they are testing,
and the executable modules of any lower-level layers of the runtime they depend
on.  On application, these modules use `Ctypes` to bind to testable functions in
the compiled executable and expose them as bound OCaml values.

A generator function has been created for each module which takes a test
function and produces a `OUnit2` test.  The test function takes both a unit test
context and an instance of the executable module under test.  The generator
function constructs a test which generates and compiles the runtime fresh for
that specific test.  This ensures that each test runs in isolation, unaffected
by the state of the previous test.

### Module Contents

The runtime contains the following modules, in roughly dependency order:

* [`Target`](#llvm-target) - LLVM module to generate into
* [`Types`](#c-types) - Standard C built-in types
* [`Libc`](#libc-bindings) - Bindings to the C standard library
* [`Gc`](#garbage-collection) - Garbage Collection
* [`Exn`](#exception-handling) - Exception Handling
* [`Json`](#json-processing) - JSON processing
* [`Xml`](#xml-processing) - XML processing
* [`Http`](#http) - HTTP Handling
* [`Crypto`](#cryptographic-hashing) - Cryptographic hashing functions
* [`System`](#top-level) - Top-level package containing the entire runtime system.

LLVM Target
---

* Implementation: Complete
* Design: To be written

C Types
---

* Implementation: In-progress (extending as needed by higher-level layers)
* Design: To be written

Libc Bindings
---

* Implementation: In-progress (extending as needed by higher-level layers)
* Design: To be written

Garbage Collection
---

* Implementation: Complete
* Design: Writing in progress

The CFN++ garbage collector is of a simple design, tailored to the peculiarities
of AWS Lambda.

Exception Handling
---

* Implementation: In-progress
* Design: To be written

JSON Processing
---

* Implementation: TODO
* Design: To be written

XML Processing
---

* Implementation: TODO
* Design: To be written

HTTP
---

* Implementation: TODO
* Design: To be written

Cryptographic Hashing
---

* Implementation: In-progress
* Design: To be written

Top Level
---

* Implementation: In-progress
* Design: To be written
