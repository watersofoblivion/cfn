(* open Format *)

open Ctypes

open Runtime

open OUnit2

let _ = Helper.init ()

let eh_test =
  let setup md =
    let syscall = Syscall.generate md in
    let libc = Libc.generate md in
    let unwind = Unwind.generate libc md in
    ignore (Exn.generate syscall libc unwind md)
  in
  Helper.llvm_test setup

let get_throw =
  let ty = Foreign.funptr (ptr void @-> ptr void @-> ptr void @-> returning void) in
  Helper.get_function ty Exn.throw_name

let get_personality =
  let ty = Foreign.funptr (void @-> returning void) in
  Helper.get_function ty Exn.personality_name

let get_begin_catch =
  let ty = Foreign.funptr (void @-> returning void) in
  Helper.get_function ty Exn.begin_catch_name

let get_end_catch =
  let ty = Foreign.funptr (void @-> returning void) in
  Helper.get_function ty Exn.end_catch_name

let test_throw =
  let test_valid ee ctxt =
    let throw = get_throw ee in
    let _ = ctxt in
    let _ = throw in
    ()
  in
  "Throw" >::: [
    "Valid" >:: eh_test test_valid
  ]

let test_personality =
  let test_valid ee ctxt =
    let personality = get_personality ee in
    let _ = ctxt in

    personality ()
  in
  "Personality Function" >::: [
    "Valid" >:: eh_test test_valid
  ]

let test_begin_catch =
  let test_valid ee ctxt =
    let begin_catch = get_begin_catch ee in
    let _ = ctxt in

    begin_catch ()
  in
  "Begin Catch" >::: [
    "Valid" >:: eh_test test_valid
  ]

let test_end_catch =
  let test_valid ee ctxt =
    let end_catch = get_end_catch ee in
    let _ = ctxt in

    end_catch ()
  in
  "End Catch" >::: [
    "Valid" >:: eh_test test_valid
  ]

let suite =
  "Exception Handling" >::: [
    test_throw;
    test_personality;
    test_begin_catch;
    test_end_catch
  ]
