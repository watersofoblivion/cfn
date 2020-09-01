open Runtime

module type Bindings = sig
  val exit : int32 -> unit
end

module Bind : functor (Syscall: Syscall.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
