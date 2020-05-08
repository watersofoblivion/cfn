open OUnit2

open Build

(* Test Initialization *)

let _ =
  try
    Sys.getenv "CFNROOT"
  with Not_found ->
    Unix.putenv "CFNROOT" "";
    ""

let cwd = Sys.getcwd ()

(* Test Initialization *)
let test_test_init =
  let test_cfn_root _ =
    try
      let _ = Sys.getenv "CFNROOT" in
      ()
    with Not_found ->
      assert_failure "${CFNROOT} not set.  Tests will fail."
  in
  "Test Initialization" >::: [
    "${CFNROOT} Environment Variable" >:: test_cfn_root
  ]

(* Constructor *)
let test_ctx =
  let test_ctx ctxt =
    let ctx = Ctx.ctx in

    assert_equal ~ctxt "" ctx.Ctx.clang;
    assert_equal ~ctxt "" ctx.stdlib;
    assert_equal ~ctxt "" ctx.home;
    assert_equal ~ctxt "" ctx.root
  in
  let test_builders =
    let test_with_clang ctxt =
      let clang_exe = Filename.temp_file "" "" in
      Unix.chmod clang_exe 0o755;

      let ctx = Ctx.ctx |> Ctx.with_clang clang_exe in
      assert_equal ~ctxt clang_exe ctx.Ctx.clang;

      Sys.remove clang_exe
    in
    let test_with_stdlib ctxt =
      let ctx = Ctx.ctx |> Ctx.with_stdlib cwd in
      assert_equal ~ctxt cwd ctx.Ctx.stdlib
    in
    let test_with_home ctxt =
      let ctx = Ctx.ctx |> Ctx.with_home cwd in
      assert_equal ~ctxt cwd ctx.Ctx.home
    in
    let test_with_root ctxt =
      let ctx = Ctx.ctx |> Ctx.with_root cwd in
      assert_equal ~ctxt cwd ctx.Ctx.root
    in
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

      (* Create a mock stdlib dir *)
      let stdlib_dir = Filename.concat root_dir "stdlib" in
      Os.mkdir stdlib_dir;

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
        let ctx = Ctx.ctx |> Ctx.from_env in
        assert_equal ~ctxt clang_exe   ctx.Ctx.clang;
        assert_equal ~ctxt home_dir    ctx.home;
        assert_equal ~ctxt stdlib_dir  ctx.stdlib;
        assert_equal ~ctxt ("/private" ^ project_dir) ctx.root
      in

      (* Set some temporary environment variables and CWD *)
      let test = test
              |> TestUtil.with_env "PATH" path
              |> TestUtil.with_env "HOME" home_dir
              |> TestUtil.with_env "CFNROOT" stdlib_dir
              |> Os.in_dir subdir
      in

      (* Run the test proper *)
      test ctxt
    in
    let _ = test_from_env in
    "Builders" >::: [
      "With Clang Executable"           >:: test_with_clang;
      "With Standard Library Directory" >:: test_with_stdlib;
      "With Home Directory"             >:: test_with_home;
      "With Project Root"               >:: test_with_root;
      "From Environment"                >:: test_from_env
    ]
  in
  "Context" >::: [
    "Constructor" >:: test_ctx;
    test_builders
  ]

(* Test Suite *)
let suite =
  "Context" >::: [
    test_test_init;
    test_ctx
  ]
