open Runtime

module type Bindings = sig
  val base_ptr : unit -> nativeint
  val reset_ptr : unit -> nativeint
  val next_ptr : unit -> nativeint
  val end_ptr : unit -> nativeint

  val from_ptr : unit -> nativeint
  val to_ptr : unit -> nativeint

  val gen_pointers : unit -> (nativeint * nativeint * nativeint * nativeint)
  val space_pointers : unit -> (nativeint * nativeint)

  val init : nativeint -> unit
  val malloc : nativeint -> unit Ctypes.ptr
  val close_perm_gen : unit -> unit
  val swap_spaces : unit -> unit
  val init_main_gen : unit -> unit
  val major : unit -> unit
end

module Bind : functor (Gc: Gc.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
