open Format

open OUnit2

open Build

(* Test Initialization *)

let _ =
  try
    Sys.getenv "CFN_HOME"
  with Not_found ->
    Unix.putenv "CFN_HOME" "";
    ""

let cwd = Sys.getcwd ()

(* Test Initialization *)
let test_test_init =
  let test_cfn_root _ =
    try
      let _ = Sys.getenv "CFN_HOME" in
      ()
    with Not_found ->
      assert_failure "${CFN_HOME} not set.  Tests will fail."
  in
  "Test Initialization" >::: [
    "${CFN_HOME} Environment Variable" >:: test_cfn_root
  ]

(* Constructor *)
let test_ctx =
  let test_empty _ =
    let ctx = Ctx.empty in

    let fn _ = Ctx.clang_exe ctx in
    assert_raises Not_found fn;

    let fn _ = Ctx.docker_exe ctx in
    assert_raises Not_found fn;

    let fn _ = Ctx.cfn_home ctx in
    assert_raises Not_found fn;

    let fn _ = Ctx.project_root ctx in
    assert_raises Not_found fn;
  in
  "Context" >::: [
    "Empty" >:: test_empty
  ]

let test_builders =
  let ctx = Ctx.empty in

  let test_with_clang =
    let test_valid ctxt =
      let expected = Filename.temp_file "" "" in
      Unix.chmod expected 0o755;
      let finally _ = Sys.remove expected in

      let fn _ =
        let ctx = Ctx.with_clang expected ctx in
        let actual = Ctx.clang_exe ctx in
        assert_equal ~ctxt expected actual;
      in
      Fun.protect ~finally fn
    in
    let test_invalid =
      let test_not_found _ =
        let path = "/i/dont/exist" in
        let msg = sprintf "%s does not exist" path in
        let exn = Invalid_argument msg in
        let fn _ = Ctx.with_clang path ctx in
        assert_raises exn fn
      in
      let test_is_dir _ =
        let dir = Os.temp_dir () in
        let finally _ = Os.rmdir dir in

        let msg = sprintf "%s is not a file" dir in
        let exn = Invalid_argument msg in
        let fn _ =
          let fn _ = Ctx.with_clang dir ctx in
          assert_raises exn fn
        in
        Fun.protect ~finally fn
      in
      let test_no_exec _ =
        let file = Filename.temp_file "" "" in
        let finally _ = Sys.remove file in

        let msg = sprintf "%s is not executable" file in
        let exn = Invalid_argument msg in
        let fn _ =
          let fn _ = Ctx.with_clang file ctx in
          assert_raises exn fn
        in
        Fun.protect ~finally fn
      in
      "Invalid" >::: [
        "Doesn't Exist"  >:: test_not_found;
        "Is Directory"   >:: test_is_dir;
        "Not Executable" >:: test_no_exec
      ]
    in
    "Clang" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  let test_with_docker =
    let test_valid ctxt =
      let expected = Filename.temp_file "" "" in
      Unix.chmod expected 0o755;
      let finally _ = Sys.remove expected in

      let fn _ =
        let ctx = Ctx.with_docker expected ctx in
        let actual = Ctx.docker_exe ctx in
        assert_equal ~ctxt expected actual;
      in
      Fun.protect ~finally fn
    in
    let test_invalid =
      let test_not_found _ =
        let path = "/i/dont/exist" in
        let msg = sprintf "%s does not exist" path in
        let exn = Invalid_argument msg in
        let fn _ = Ctx.with_docker path ctx in
        assert_raises exn fn
      in
      let test_is_dir _ =
        let dir = Os.temp_dir () in
        let finally _ = Os.rmdir dir in

        let msg = sprintf "%s is not a file" dir in
        let exn = Invalid_argument msg in
        let fn _ =
          let fn _ = Ctx.with_docker dir ctx in
          assert_raises exn fn
        in
        Fun.protect ~finally fn
      in
      let test_no_exec _ =
        let file = Filename.temp_file "" "" in
        let finally _ = Sys.remove file in

        let msg = sprintf "%s is not executable" file in
        let exn = Invalid_argument msg in
        let fn _ =
          let fn _ = Ctx.with_docker file ctx in
          assert_raises exn fn
        in
        Fun.protect ~finally fn
      in
      "Invalid" >::: [
        "Doesn't Exist"  >:: test_not_found;
        "Is Directory"   >:: test_is_dir;
        "Not Executable" >:: test_no_exec
      ]
    in
    "Docker" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  let test_with_cfn_home =
    let test_valid ctxt =
      let ctx = Ctx.with_cfn_home cwd ctx in
      let actual = Ctx.cfn_home ctx in
      assert_equal ~ctxt cwd actual
    in
    let test_invalid =
      let test_not_found _ =
        let path = "/i/dont/exist" in
        let msg = sprintf "%s does not exist" path in
        let exn = Invalid_argument msg in
        let fn _ = Ctx.with_cfn_home path ctx in
        assert_raises exn fn
      in
      let test_not_dir _ =
        let file = Filename.temp_file "" "" in
        let finally _ = Sys.remove file in

        let msg = sprintf "%s is not a directory" file in
        let exn = Invalid_argument msg in
        let fn _ =
          let fn _ = Ctx.with_cfn_home file ctx in
          assert_raises exn fn
        in
        Fun.protect ~finally fn
      in
      "Invalid" >::: [
        "Doesn't Exist"   >:: test_not_found;
        "Not a Directory" >:: test_not_dir
      ]
    in
    "CFN++ Home Directory" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  let test_with_root =
    let test_valid ctxt =
      let ctx = Ctx.with_project_root cwd ctx in
      let actual = Ctx.project_root ctx in
      assert_equal ~ctxt cwd actual
    in
    let test_invalid =
      let test_not_found _ =
        let path = "/i/dont/exist" in
        let msg = sprintf "%s does not exist" path in
        let exn = Invalid_argument msg in
        let fn _ = Ctx.with_project_root path ctx in
        assert_raises exn fn
      in
      let test_not_dir _ =
        let file = Filename.temp_file "" "" in
        let finally _ = Sys.remove file in

        let msg = sprintf "%s is not a directory" file in
        let exn = Invalid_argument msg in
        let fn _ =
          let fn _ = Ctx.with_project_root file ctx in
          assert_raises exn fn
        in
        Fun.protect ~finally fn
      in
      "Invalid" >::: [
        "Doesn't Exist"   >:: test_not_found;
        "Not a Directory" >:: test_not_dir
      ]
    in
    "Project Root" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  "Builders" >::: [
    test_with_clang;
    test_with_docker;
    test_with_cfn_home;
    test_with_root;
  ]

let test_from_env ctxt =
  let tmpdir = Filename.get_temp_dir_name () in
  let root_dir = Filename.concat tmpdir "root" in

  (* Create mock /usr/bin and /usr/local/bin and the ${PATH} environment variable. *)
  let usr_bin = "bin"
             |> Filename.concat "usr"
             |> Filename.concat root_dir
  in
  let usr_local_bin = "bin"
                   |> Filename.concat "local"
                   |> Filename.concat "usr"
                   |> Filename.concat root_dir
  in
  let path = String.concat ":" [usr_bin; usr_local_bin] in
  Os.mkdir usr_bin;
  Os.mkdir usr_local_bin;

  (* Create a mock clang executable in /usr/local/bin *)
  let clang_exe = Filename.concat usr_local_bin "clang" in
  let oc = open_out clang_exe in
  close_out oc;
  Unix.chmod clang_exe 0o755;

  (* Create a mock docker executable in /usr/local/bin *)
  let docker_exe = Filename.concat usr_local_bin "docker" in
  let oc = open_out docker_exe in
  close_out oc;
  Unix.chmod docker_exe 0o755;

  (* Create a mock CFN++ home dir *)
  let home_dir = Filename.concat root_dir "stdlib" in
  Os.mkdir home_dir;

  (* Create a mock home directory *)
  let home_dir = "username"
              |> Filename.concat "home"
              |> Filename.concat root_dir
  in
  Os.mkdir home_dir;

  (* Create a mock project *)
  let project_dir = "test-project"
                 |> Filename.concat "projects"
                 |> Filename.concat home_dir
  in
  Os.mkdir project_dir;

  let subdir = Filename.concat project_dir "subdir" in
  Os.mkdir subdir;

  let project_file = Filename.concat project_dir "project.json" in
  let oc = open_out project_file in
  close_out oc;

  (* The test proper *)
  let test ctxt =
    let ctx = Ctx.from_env Ctx.empty in

    let actual = Ctx.clang_exe ctx in
    assert_equal ~ctxt clang_exe actual;

    let actual = Ctx.docker_exe ctx in
    assert_equal ~ctxt docker_exe actual;

    let actual = Ctx.cfn_home ctx in
    assert_equal ~ctxt home_dir actual;

    let actual = Ctx.project_root ctx in
    assert_equal ~ctxt ("/private" ^ project_dir) actual
  in

  (* Set some temporary environment variables and CWD *)
  let test = test
          |> TestUtil.with_env "PATH" path
          |> TestUtil.with_env "HOME" home_dir
          |> TestUtil.with_env "CFN_HOME" home_dir
          |> Os.in_dir subdir
  in

  (* Run the test proper *)
  test ctxt

let test_path_helpers =
  let root = Sys.getcwd () in
  let home = Sys.getcwd () in
  let ctx = Ctx.empty
         |> Ctx.with_project_root root
         |> Ctx.with_cfn_home home
  in

  let test_cfn_stdlib ctxt =
    let expected = Filename.concat home Ctx.cfn_stdlib_dir in
    let actual = Ctx.cfn_stdlib ctx in

    assert_equal ~ctxt expected actual
  in
  let test_cfn_shared ctxt =
    let expected = Filename.concat home Ctx.cfn_shared_dir in
    let actual = Ctx.cfn_shared ctx in

    assert_equal ~ctxt expected actual
  in
  let test_build_path ctxt =
    let expected = Filename.concat root Ctx.build_dir in
    let actual = Ctx.build_path ctx in

    assert_equal ~ctxt expected actual
  in
  let test_src_path ctxt =
    let build_path = Ctx.build_path ctx in
    let expected = Filename.concat build_path Ctx.src_dir in
    let actual = Ctx.src_path ctx in

    assert_equal ~ctxt expected actual
  in
  let test_lib_path ctxt =
    let build_path = Ctx.build_path ctx in
    let expected = Filename.concat build_path Ctx.lib_dir in
    let actual = Ctx.lib_path ctx in

    assert_equal ~ctxt expected actual
  in
  "Path Helpers" >::: [
    "Standard Library" >:: test_cfn_stdlib;
    "Shared Resources" >:: test_cfn_shared;
    "Build Path"       >:: test_build_path;
    "Sources Path"     >:: test_src_path;
    "Packages Path"    >:: test_lib_path
  ]

(* Test Suite *)
let suite =
  "Context" >::: [
    test_test_init;
    test_ctx;
    test_builders;
    "From Environment" >:: test_from_env;
    test_path_helpers
  ]
