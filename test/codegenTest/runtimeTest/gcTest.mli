open Ctypes

open Runtime

module type Bindings = sig
  val base_ptr : unit -> Unsigned.uint64
  val reset_ptr : unit -> Unsigned.uint64
  val next_ptr : unit -> Unsigned.uint64
  val end_ptr : unit -> Unsigned.uint64

  val from_ptr : unit -> Unsigned.uint64
  val to_ptr : unit -> Unsigned.uint64

  val gen_pointers : unit -> (Unsigned.uint64 * Unsigned.uint64 * Unsigned.uint64 * Unsigned.uint64)
  val space_pointers : unit -> (Unsigned.uint64 * Unsigned.uint64)

  val init : int64 -> unit
  val malloc : int64 -> (int64, [`C]) pointer
  val close_perm_gen : unit -> unit
  val swap_spaces : unit -> unit
  val init_main_gen : unit -> unit
  val major : unit -> unit
end

module Bind : functor (Gc: Gc.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
