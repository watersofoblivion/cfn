open System

open OUnit2

(* Tests *)

let test_command =
  let test_valid _ =
    match Docker.docker ["--help"] Os.lines with
      | [] -> assert_failure "expected output"
      | _ -> ()
  in
  let test_invalid ctxt =
    let fn _ = Docker.docker ["invalid-command"] Os.ignore in
    let stderr =
      let msg =
        "docker: 'invalid-command' is not a docker command.\n" ^
        "See 'docker --help'\n"
      in
      Some msg
    in
    OsTest.assert_non_zero ~ctxt ~stderr 1 fn
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
  "Docker" >::: [
    test_command;
    test_image;
    test_container;
    test_run
  ]
