open Runtime

module type Bindings = sig
  val int_to_ptr : nativeint -> unit Ctypes.ptr
  val ptr_to_int : unit Ctypes.ptr -> nativeint
end

module Bind : functor (Types: Types.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
