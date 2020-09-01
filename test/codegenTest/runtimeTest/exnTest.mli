open Runtime

module type Bindings = sig
  val throw : unit Ctypes.ptr -> unit Ctypes.ptr -> unit Ctypes.ptr -> unit
  val personality : unit -> unit
  val begin_catch : unit -> unit
  val end_catch : unit -> unit
end

module Bind : functor (Exn: Exn.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
