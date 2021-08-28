open Runtime

module type Bindings = sig
  val malloc : nativeint -> unit Ctypes.ptr
  val memcpy : unit Ctypes.ptr -> unit Ctypes.ptr -> nativeint -> unit Ctypes.ptr
  val exit : int32 -> unit
end

module Bind : functor (Libc: Libc.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
