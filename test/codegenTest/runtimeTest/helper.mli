open Llvm
open Llvm_executionengine
open Ctypes

open OUnit2

val init : unit -> unit

val get_global_var : 'a typ -> string -> llexecutionengine -> 'a
val get_function : 'a typ -> string -> llexecutionengine -> 'a

val get_global_uint64_var : string -> llexecutionengine -> Unsigned.uint64

val llvm_test : (llmodule -> unit) -> (llexecutionengine -> test_ctxt -> unit) -> test_ctxt -> unit
