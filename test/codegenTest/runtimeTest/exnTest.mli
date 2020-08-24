open Ctypes

open Runtime

module type Bindings = sig
  val throw : (unit, [`C]) pointer -> (unit, [`C]) pointer -> (unit, [`C]) pointer -> unit
  val personality : unit -> unit
  val begin_catch : unit -> unit
  val end_catch : unit -> unit
end

module Bind : functor (Asm: Exn.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
