(* open Format *)

open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val throw : (unit, [`C]) pointer -> (unit, [`C]) pointer -> (unit, [`C]) pointer -> unit
  val personality : unit -> unit
  val begin_catch : unit -> unit
  val end_catch : unit -> unit
end

module Bind (Asm: Exn.Asm) (Exe: TargetTest.Exe) = struct
  let throw =
    let ty = Foreign.funptr (ptr void @-> ptr void @-> ptr void @-> returning void) in
    Exe.func ty Asm.Names.throw

  let personality =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.personality

  let begin_catch =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.begin_catch

  let end_catch =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.end_catch
end

let exn_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in
    let module Syscall = Syscall.Generate (Types) (Target) in
    let module Libc = Libc.Generate (Types) (Target) in
    let module Unwind = Unwind.Generate (Types) (Target) in

    let module Asm = Exn.Generate (Types) (Syscall) (Unwind) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Exn = Bind (Asm) (Exe) in
    test_fn (module Exn: Bindings))

let test_throw =
  let test_valid (module Exn: Bindings) ctxt =
    let _ = ctxt in
    ()
  in
  "Throw" >::: [
    "Valid" >:: exn_test test_valid
  ]

let test_personality =
  let test_valid (module Exn: Bindings) ctxt =
    let _ = ctxt in

    Exn.personality ()
  in
  "Personality Function" >::: [
    "Valid" >:: exn_test test_valid
  ]

let test_begin_catch =
  let test_valid (module Exn: Bindings) ctxt =
    let _ = ctxt in

    Exn.begin_catch ()
  in
  "Begin Catch" >::: [
    "Valid" >:: exn_test test_valid
  ]

let test_end_catch =
  let test_valid (module Exn: Bindings) ctxt =
    let _ = ctxt in

    Exn.end_catch ()
  in
  "End Catch" >::: [
    "Valid" >:: exn_test test_valid
  ]

let suite =
  "Exception Handling" >::: [
    test_throw;
    test_personality;
    test_begin_catch;
    test_end_catch
  ]
