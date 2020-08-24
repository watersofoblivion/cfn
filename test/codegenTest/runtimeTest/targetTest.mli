open Ctypes
open Llvm_executionengine

open Runtime

open OUnit2

module type Exe = sig
  val ee : llexecutionengine

  val global : 'a typ -> string -> 'a
  val func : 'a typ -> string -> 'a
end

module Compile : functor (Asm: Target.Asm) -> Exe

val test : ((module Target.Asm) -> test_ctxt -> unit) -> test_ctxt -> unit
