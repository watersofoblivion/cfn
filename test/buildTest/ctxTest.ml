open Format

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

(* Helpers *)
let with_env k v f x =
  let old =
    try Unix.getenv k
    with Not_found -> ""
  in
  Unix.putenv k v;

  try
    let res = f x in
    Unix.putenv k old;
    res
  with exn ->
    Unix.putenv k old;
    raise exn


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

(* Test Helpers *)
let test_test_helpers =
  let var_name = "__TEST_VARIABLE__" in
  let external_value = "external-value" in
  let internal_value = "internal-value" in

  Unix.putenv var_name external_value;

  let test_with_env ctxt =
    let fn internal_value =
      try
        let actual = Sys.getenv var_name in
        assert_equal ~ctxt internal_value actual
      with Not_found ->
        assert_failure (sprintf "${%s} not set" var_name)
    in
    with_env var_name internal_value fn internal_value;

    try
      let actual = Sys.getenv var_name in
      assert_equal ~ctxt external_value actual
    with Not_found ->
      assert_failure (sprintf "${%s} not set" var_name)
  in
  "Test Helpers" >::: [
    "With Environment Variable" >:: test_with_env
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
      (* Create mock /usr/bin and /usr/local/bin and the ${PATH} environment variable. *)
      let usr_bin = "bin"
                 |> Filename.concat "usr"
                 |> Filename.concat cwd
      in
      Os.mkdir usr_bin;
      let usr_local_bin = "bin"
                       |> Filename.concat "local"
                       |> Filename.concat "usr"
                       |> Filename.concat cwd
      in
      Os.mkdir usr_local_bin;
      let path = String.concat ":" [usr_bin; usr_local_bin] in

      (* Create a mock clang executable in /usr/local/bin *)
      let clang_exe = Filename.concat usr_local_bin "clang" in
      let oc = open_out clang_exe in
      close_out oc;
      Unix.chmod clang_exe 0o755;

      (* Create a mock stdlib dir *)
      let stdlib_dir = Filename.concat cwd "stdlib" in
      Os.mkdir stdlib_dir;

      (* Create a mock home directory *)
      let home_dir = "username"
                  |> Filename.concat "home"
                  |> Filename.concat cwd
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
        assert_equal ~ctxt project_dir ctx.root
      in

      (* Set some temporary environment variables and CWD *)
      let test = test
              |> with_env "PATH" path
              |> with_env "HOME" home_dir
              |> with_env "CFNROOT" stdlib_dir
              |> Os.in_dir subdir
      in

      (* Run the test proper *)
      test ctxt
    in
    "Builders" >::: [
      "With Clang Executable"           >:: test_with_clang;
      "With Standard Library Directory" >:: test_with_stdlib;
      "With Home Directory"             >:: test_with_home;
      "With Project Root"               >:: test_with_root;
      "From Environment"                >:: (test_from_env |> Os.in_temp_dir)
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
    test_test_helpers;
    test_ctx
  ]
