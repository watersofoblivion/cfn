open Runtime

module type Bindings = sig
  type e

  val raise_exception : e -> unit
end

module Bind : functor (Asm: Unwind.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
