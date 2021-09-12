open Format

open System

open OUnit2

(* Assertions *)

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
      Some (
        "docker: 'invalid-command' is not a docker command.\n" ^
        "See 'docker --help'\n"
      )
    in
    OsTest.assert_non_zero ~ctxt ~stderr 1 fn
  in
  "Command" >::: [
    "Valid"   >:: test_valid;
    "Invalid" >:: test_invalid
  ]

let assert_named_string msg = Assert.string_equals ~msg:msg

let test_image =
  let image_dir ctxt image = in_testdata_dir ctxt ["toolchainTest"; "systemTest"; "images"; image] in

  let should_have_name =
    let assert_name = assert_named_string "Image Name" in
    Assert.should assert_name Docker.img_name
  in
  let should_have_tag =
    let assert_tag = assert_named_string "Image Tag" in
    Assert.should assert_tag Docker.img_tag
  in

  let name = "test-name" in
  let tag = "test-tag" in

  let test_constructor =
    let test_valid ctxt =
      Docker.image name tag
        |> should_have_name ~ctxt name
        |> should_have_tag ~ctxt tag
        |> Assert.success
    in
    "Constructor" >::: [
      "Valid" >:: test_valid
    ]
  in

  let test_build =
    let test_success ctxt =
      let dir = image_dir ctxt "valid-image" in

      Docker.build dir name tag
        |> should_have_name ~ctxt name
        |> should_have_tag ~ctxt tag
        |> Assert.success
    in
    let test_failure =
      let test_unknown_image ctxt =
        let dir = image_dir ctxt "unknown-image" in
        let stderr = Some "pull access denied for image-that-dosent-exist, repository does not exist or may require 'docker login'\n" in

        (fun _ -> ignore (Docker.build dir name tag))
          |> OsTest.assert_non_zero ~ctxt ~stderr 1
      in
      let test_unknown_tag ctxt =
        let dir = image_dir ctxt "unknown-tag" in
        let stderr = Some "manifest for amazonlinux:tag-that-doesnt-exist not found\n" in

        (fun _ -> ignore (Docker.build dir name tag))
          |> OsTest.assert_non_zero ~ctxt ~stderr 1
      in
      let test_invalid_image ctxt =
        let dir = image_dir ctxt "invalid-image" in
        let stderr = Some "Error response from daemon: Dockerfile parse error line 1: unknown instruction: FORM\n" in

        (fun _ -> ignore (Docker.build dir name tag))
          |> OsTest.assert_non_zero ~ctxt ~stderr 1
      in
      let test_invalid_tag ctxt =
        let dir = image_dir ctxt "valid-image" in
        let name = "invalid#name" in
        let tag = "invalid?tag" in
        let stderr =
          Some (
            "invalid argument \"invalid#name:invalid?tag\" for t: invalid reference format\n" ^
            "See 'docker build --help'.\n"
          )
        in

        (fun _ -> ignore (Docker.build dir name tag))
          |> OsTest.assert_non_zero ~ctxt ~stderr 125
      in
      "Failure" >::: [
        "Unknown Image" >:: test_unknown_image;
        "Unknown Tag"   >:: test_unknown_tag;
        "Invalid Image" >:: test_invalid_image;
        "Invalid Tag"   >:: test_invalid_tag
      ]
    in
    "Build" >::: [
      "Success"  >:: test_success;
      test_failure
    ]
  in
  "Image" >::: [
    test_constructor;
    test_build
  ]

let test_mount =
  let should_have_local_dir =
    let assert_local_dir = assert_named_string "Local Dir" in
    Assert.should assert_local_dir Docker.local_dir
  in
  let should_have_container_dir =
    let assert_container_dir = assert_named_string "Container Dir" in
    Assert.should assert_container_dir Docker.container_dir
  in

  let test_mount =
    let test_dir_mount =
      let container_dir = "/tmp" in

      let test_valid ctxt =
        let cwd = Sys.getcwd () in

        let local_dir = Filename.concat cwd "local_dir" in
        Os.mkdir local_dir;

        Docker.dir_mount local_dir container_dir
          |> should_have_local_dir ~ctxt local_dir
          |> should_have_container_dir ~ctxt container_dir
          |> Assert.success
      in
      let test_invalid =
        let local_dir = "local-dir" in

        let assert_non_local msg dir =
          let msg = sprintf "%s dir %S is not absolute" msg dir in
          Assert.invalid_argument msg
        in

        let test_non_existent_local _ =
          let cwd = Sys.getcwd () in
          let local_dir = Filename.concat cwd local_dir in

          let msg = sprintf "%S does not exist" local_dir in
          (fun _ -> ignore (Docker.dir_mount local_dir container_dir))
            |> Assert.invalid_argument msg
        in
        let test_non_directory_local _ =
          let cwd = Sys.getcwd () in
          let local_dir = Filename.concat cwd local_dir in
          let oc = open_out local_dir in
          close_out oc;

          let msg = sprintf "%S is not a directory" local_dir in
          (fun _ -> ignore (Docker.dir_mount local_dir container_dir))
            |> Assert.invalid_argument msg
        in
        let test_non_absolute_local _ =
          Os.mkdir local_dir;

          (fun _ -> ignore (Docker.dir_mount local_dir container_dir))
            |> assert_non_local "local" local_dir
        in
        let test_non_absolute_container _ =
          let cwd = Sys.getcwd () in
          let local_dir = Filename.concat cwd local_dir in
          Os.mkdir local_dir;

          let container_dir = "non-absolute-dir" in

          (fun _ -> ignore (Docker.dir_mount local_dir container_dir))
            |> assert_non_local "container" container_dir
        in
        "Invalid" >::: [
          "Non-existent Local Directory"     >:: Os.in_temp_dir test_non_existent_local;
          "Non-directory Local Directory"    >:: Os.in_temp_dir test_non_directory_local;
          "Non-absolute Local Directory"     >:: Os.in_temp_dir test_non_absolute_local;
          "Non-absolute Container Directory" >:: Os.in_temp_dir test_non_absolute_container
        ]
      in
      "Directory Mount" >::: [
        "Valid" >:: Os.in_temp_dir test_valid;
        test_invalid
      ]
    in
    "Constructor" >::: [
      test_dir_mount
    ]
  in
  "Mounts" >::: [
    test_mount
  ]

let test_run =
  let test_run_in =
    let image_dir ctxt image = in_testdata_dir ctxt ["toolchainTest"; "systemTest"; "images"; image] in

    let test_valid =
      let test_exit_status ctxt =
        let dir = image_dir ctxt "valid-image" in
        let img = Docker.build dir "test-image" "test-tag" in

        Docker.run_in "/bin/true" [] img Os.ignore
          |> Assert.success
      in
      let test_mount ctxt =
        let cwd = Sys.getcwd () in
        let container_dir = "/tmp" in

        let dir = image_dir ctxt "valid-image" in
        let img = Docker.build dir "test-image" "test-tag" in

        let contents = "The contents" in

        let source_filename = "source.txt" in
        let local_source_path = Filename.concat cwd source_filename in
        let container_source_path = Filename.concat container_dir source_filename in

        let target_filename = "target.txt" in
        let local_target_path = Filename.concat cwd target_filename in
        let container_target_path = Filename.concat container_dir target_filename in

        let mounts =
          let dir = Docker.dir_mount cwd container_dir in
          Docker.mounts
            |> Docker.add_mount dir
        in

        let oc = open_out local_source_path in
        let finally _ = close_out oc in
        Fun.protect ~finally (fun _ -> output_string oc contents);

        (* TODO *)
        Docker.run_in "cp" [container_source_path; container_target_path] ~mounts img Os.ignore
          |> Assert.success;

        let msg = sprintf "expected file %S to exist" local_target_path in
        local_target_path
          |> Sys.file_exists
          |> assert_bool msg;

        let ic = open_in local_target_path in
        let finally _ = close_in ic in
        Fun.protect ~finally (fun _ ->
          ic
            |> input_line
            |> Assert.string_equals ~ctxt ~msg:"Contents" contents
            |> Assert.success)
      in
      "Valid" >::: [
        "Exit Status" >:: test_exit_status;
        "Mounts"      >:: test_mount
      ]
    in
    let test_invalid =
      let test_exit_status ctxt =
        let dir = image_dir ctxt "valid-image" in
        let img = Docker.build dir "test-image" "test-tag" in

        let stdout = Some "" in
        let stderr = Some "" in

        (fun _ -> ignore (Docker.run_in "/bin/false" [] img Os.ignore))
          |> OsTest.assert_non_zero ~ctxt ~stdout ~stderr 1
          |> Assert.success
      in
      "Invalid" >::: [
        "Exit Status" >:: test_exit_status;
      ]
    in
    "Command In Container" >::: [
      test_valid;
      test_invalid
    ]
  in
  "Run" >::: [
    test_run_in
  ]

(* Test Suite *)
let suite =
  "Docker" >::: [
    test_command;
    test_image;
    test_mount;
    test_run
  ]
