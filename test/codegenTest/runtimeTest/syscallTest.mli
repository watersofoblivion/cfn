open Runtime

module type Bindings = sig
  val exit : unit -> unit
end

module Bind : functor (Asm: Syscall.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
