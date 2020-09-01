open Ctypes

open Runtime

open OUnit2

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

module Bind (Unwind: Unwind.Asm) (Exe: TargetTest.Exe) = struct
  type e = unit

  let raise_exception =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.raise_exception

  let get_language_specific_data =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.get_language_specific_data

  let get_region_start =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.get_region_start

  let get_ip =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.get_ip

  let set_ip =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.set_ip

  let set_gr =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.set_gr

  let resume =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Unwind.Names.resume
end

let unwind_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in

    let module Asm = Unwind.Generate (Types) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Unwind = Bind (Asm) (Exe) in
    test_fn (module Unwind: Bindings))

let _ = unwind_test

let suite =
  "Unwind" >::: [
  ]
