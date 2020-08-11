(* open Format

open OUnit2

open Artifact

(* Test Initialization *)

let _ =
  try
    Sys.getenv "CFN_HOME"
  with Not_found ->
    Unix.putenv "CFN_HOME" "";
    ""

(* let cwd = Sys.getcwd () *)

(* Test Initialization *)
(* let test_test_init =
  let test_cfn_root _ =
    try
      let _ = Sys.getenv "CFN_HOME" in
      ()
    with Not_found ->
      assert_failure "${CFN_HOME} not set.  Tests will fail."
  in
  "Test Initialization" >::: [
    "${CFN_HOME} Environment Variable" >:: test_cfn_root
  ] *)

(* Workspaces *)

let test_paths =
  let assert_not_blank ~ctxt str =
    let cmp x y = not (x = y) in
    assert_equal ~ctxt ~cmp "" str
  in

  let test_file_names =
    let test_project_file_name ctxt =
      Workspace.project_file_name
        |> assert_not_blank ~ctxt
    in
    let test_lock_file_name ctxt =
      Workspace.lock_file_name
        |> assert_not_blank ~ctxt
    in
    let test_anf_file_name ctxt =
      Workspace.anf_file_name
        |> assert_not_blank ~ctxt
    in
    "File Names" >::: [
      "Project File" >:: test_project_file_name;
      "Lock File"    >:: test_lock_file_name;
      "ANF File"     >:: test_anf_file_name
    ]
  in
  let test_directories =
    let test_cache_dir ctxt =
      Workspace.cache_dir
        |> assert_not_blank ~ctxt
    in
    let test_src_dir ctxt =
      Workspace.src_dir
        |> assert_not_blank ~ctxt
    in
    let test_lib_dir ctxt =
      Workspace.lib_dir
        |> assert_not_blank ~ctxt
    in
    let test_pkg_dir ctxt =
      Workspace.pkg_dir
        |> assert_not_blank ~ctxt
    in
    "Directories" >::: [
      "Cache"              >:: test_cache_dir;
      "Source"             >:: test_src_dir;
      "Compiled Artifacts" >:: test_lib_dir;
      "Packages"           >:: test_pkg_dir
    ]
  in
  "Paths" >::: [
    test_file_names;
    test_directories;
  ]

let test_create =
  let id = Path.id "github.com/test/project" in

  let test_valid =
    let test_valid ctxt =
      let fn _ =
        let id = Path.id "github.com/test/project" in

        let cwd = Sys.getcwd () in
        let path = Filename.concat cwd "test-workspace" in

        let actual = Workspace.create id path in
        let fn _ = Workspace.current () in
        let expected = Os.in_dir path fn () in

        assert_equal ~ctxt expected actual
      in

      let temp_dir = Os.temp_dir () in
      let finally _ = Os.rmdir temp_dir in
      let fn = Os.in_dir temp_dir fn in
      Fun.protect ~finally fn
    in
    "Valid" >::: [
      "Valid" >:: test_valid
    ]
  in
  let test_invalid =
    let test_relative_path _ =
      let path = "relative/path" in
      let msg = sprintf "%s is not an absolute path" path in
      let exn = Invalid_argument msg in
      let fn _ = Workspace.create id path in
      assert_raises exn fn
    in
    let test_dir_exists _ =
      let path = Os.temp_dir () in
      let finally _ = Os.rmdir path in

      let fn _ =
        let msg = sprintf "%s exists" path in
        let exn = Invalid_argument msg in

        let fn _ = Workspace.create id path in
        assert_raises exn fn
      in

      Fun.protect ~finally fn
    in
    "Invalid" >::: [
      "Relative Path"    >:: test_relative_path;
      "Directory Exists" >:: test_dir_exists
    ]
  in
  "Create" >::: [
    test_valid;
    test_invalid
  ]

let test_current =
  "Current" >::: [
  ]

let suite =
  "Workspaces" >::: [
    test_paths;
    test_create;
    test_current
  ] *)

open OUnit2
let suite = "Workspaces" >::: []
