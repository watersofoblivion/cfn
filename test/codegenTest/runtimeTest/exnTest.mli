open Runtime

module type Bindings = sig
  module Dwarf : sig
    val uleb128_decode : unit Ctypes.ptr -> Unsigned.uint64
    val sleb128_decode : unit Ctypes.ptr -> int64
  end

  val personality : int32 -> int32 -> Unsigned.uint64 -> unit Ctypes.ptr -> unit Ctypes.ptr -> int32
end

module Bind : functor (Exn: Exn.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
