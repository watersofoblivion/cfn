open System

open OUnit2

(* Tests *)

let test_command =
  let test_valid _ =
    match Docker.docker ["--help"] Os.lines with
      | [] -> assert_failure "expected output"
      | _ -> ()
  in
  let test_invalid _ =
    let fn _ =
      Docker.docker ["invalid-command"] Os.ignore
    in
    let exn = Failure "exited with status 1" in
    assert_raises exn fn
  in
  "Command" >::: [
    "Valid"   >:: test_valid;
    "Invalid" >:: test_invalid
  ]

let test_image =
  "Image" >::: [
  ]

let test_container =
  "Container" >::: [
  ]

let test_run =
  "Run" >::: [
  ]

(* Test Suite *)
let suite =
  "Git" >::: [
    test_command;
    test_image;
    test_container;
    test_run
  ]
