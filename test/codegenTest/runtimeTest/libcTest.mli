open Runtime

module type Bindings = sig
  val malloc : nativeint -> unit Ctypes.ptr
end

module Bind : functor (Libc: Libc.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
