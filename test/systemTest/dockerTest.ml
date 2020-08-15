open System

open OUnit2

(* Assertions *)

let should_have_img_name ~ctxt name img =
  let _ =
    img
      |> Docker.img_name
      |> Assert.string_equals ~ctxt name
  in
  img

let should_have_img_tag ~ctxt tag img =
  let _ =
    img
      |> Docker.img_tag
      |> Assert.string_equals ~ctxt tag
  in
  img

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
        "docker: 'invalid-command' asdfasdf is not a docker command.\n" ^
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
  let test_image_dir ctxt = in_testdata_dir ctxt ["systemTest/images/testimage"] in

  let test_build =
    let test_success ctxt =
      let dir = test_image_dir ctxt in
      let name = "test-name" in
      let tag = "test-tag" in

      Docker.build dir name tag
        |> should_have_img_name ~ctxt name
        |> should_have_img_tag ~ctxt tag
        |> Assert.success
    in
    "Build" >::: [
      "Success"  >:: test_success
    ]
  in
  "Image" >::: [
    test_build
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
