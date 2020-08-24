open Ctypes

open Runtime

module type Bindings = sig
  val malloc : int64 -> (int64, [`C]) pointer
end

module Bind : functor (Asm: Libc.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
