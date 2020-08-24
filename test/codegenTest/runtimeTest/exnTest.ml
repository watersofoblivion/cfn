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

let exn_test tester ctxt =
  let ctx = Llvm.create_context () in
  let finally _ = Llvm.dispose_context ctx in
  let fn _ =
    let module Target = struct
      module Names = struct
        let prefix = "cfn++"
      end
      let ctx = ctx
      let md = Llvm.create_module ctx "test-module"
    end in

    let module Syscall = Syscall.Generate (Target) in
    let module Libc = Libc.Generate (Target) in
    let module Unwind = Unwind.Generate (Libc) (Target) in

    let module Asm = Exn.Generate (Syscall) (Libc) (Unwind) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Exn = Bind (Asm) (Exe) in
    tester (module Exn: Bindings) ctxt
  in
  Fun.protect ~finally fn

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
