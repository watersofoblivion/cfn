open Runtime

module type Bindings = sig
  type e

  val raise_exception : e -> unit
  val get_language_specific_data : unit -> unit
  val get_region_start : unit -> unit
  val get_ip : unit -> unit
  val set_ip : unit -> unit
  val set_gr : unit -> unit
  val resume : unit -> unit
end

module Bind : functor (Unwind: Unwind.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
