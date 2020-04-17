open Format
open OUnit2
open Cfn

(* Test Initialization *)

let _ =
  try
    Sys.getenv "CFNROOT"
  with Not_found ->
    Unix.putenv "CFNROOT" "";
    ""

let _ = Random.self_init ()

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


(* Useful globals *)
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

(* Filesystem Helpers *)
let test_fs_helpers =
  let test_mkpath =
    let test_with_empty_path ctxt =
      let actual = Build.mkpath [] in
      assert_equal ~ctxt cwd actual
    in
    let test_with_absolute_path ctxt =
      let expected = "/usr/bin" in
      let actual = Build.mkpath ["/usr"; "bin"] in
      assert_equal ~ctxt expected actual
    in
    let test_with_implicit_path ctxt =
      let expected = "bar"
                  |> Filename.concat "foo"
                  |> Filename.concat cwd
      in
      let actual = Build.mkpath ["foo"; "bar"] in
      assert_equal ~ctxt expected actual
    in
    let test_with_current_directory ctxt =
      let actual = Build.mkpath ["."] in
      assert_equal ~ctxt cwd actual;

      let actual = Build.mkpath ["./"] in
      assert_equal ~ctxt cwd actual;

      let expected = "bar"
                  |> Filename.concat "foo"
                  |> Filename.concat cwd
      in
      let actual =
        Build.mkpath [Filename.concat Filename.current_dir_name "foo"; "bar"]
      in
      assert_equal ~ctxt expected actual
    in
    let test_with_parent_directory ctxt =
      let expected = Filename.dirname cwd in

      let actual = Build.mkpath [".."] in
      assert_equal ~ctxt expected actual;

      let actual = Build.mkpath ["../"] in
      assert_equal ~ctxt expected actual;

      let expected = "bar"
                  |> Filename.concat "foo"
                  |> Filename.concat expected
      in
      let actual =
        Build.mkpath [Filename.concat Filename.parent_dir_name "foo"; "bar"]
      in
      assert_equal ~ctxt expected actual
    in
    "Make Path" >::: [
      "With Empty Path"        >:: test_with_empty_path;
      "With Absolute Path"     >:: test_with_absolute_path;
      "With Implicit Path"     >:: test_with_implicit_path;
      "With Current Directory" >:: test_with_current_directory;
      "With Parent Directory"  >:: test_with_parent_directory
    ]
  in
  let test_mkdir_p =
    let tmpdir = Filename.get_temp_dir_name () in
    let assert_executable_dir path =
      assert_bool (sprintf "%S is not a directory" path) (Sys.is_directory path);
      let stat = Unix.stat path in
      assert_bool
        (sprintf "%S does not have mode 0o755" path)
        (stat.st_perm = 0o755)
    in

    let test_valid ctxt =
      let expected = "bar"
                  |> Filename.concat "foo"
                  |> Filename.concat tmpdir
      in
      let fn _ =
        let path = Filename.concat "foo" "bar" in
        Build.mkdir_p path
      in
      let actual = Build.in_dir tmpdir fn () in

      (* TODO: Fix the "/private" and trailing "/" problem for real. *)
      let printer path = path in
      assert_equal ~ctxt ~printer (String.concat "" ["/private"; expected]) actual;

      assert_executable_dir actual;
      assert_executable_dir (Filename.dirname actual)
    in
    let test_partial ctxt =
      let expected = "dir_3"
                  |> Filename.concat "dir_1"
                  |> Filename.concat tmpdir
      in
      let partial = "dir_2"
                 |> Filename.concat "dir_1"
                 |> Filename.concat tmpdir
      in
      let _ = Build.mkdir_p partial in
      let actual = Build.mkdir_p expected in

      assert_equal ~ctxt expected actual;
      assert_executable_dir actual;
      assert_executable_dir (Filename.dirname actual)
    in
    let test_not_dir _ =
      let file_path = Filename.concat tmpdir "file" in
      let oc = open_out file_path in
      close_out oc;

      try
        let dir_path = "error"
                    |> Filename.concat "file"
                    |> Filename.concat tmpdir
        in
        let _ = Build.mkdir_p dir_path in
        let msg = sprintf "expected %S to fail because %S is a file" dir_path file_path in
        assert_failure msg
      with Failure _ -> ()
    in
    let test_exn _ =
      let path = Build.mkpath ["/usr"; "is-not-world-writable"] in
      let exn = Unix.Unix_error (Unix.EPERM, "mkdir", path) in
      let fn _ = Build.mkdir_p path in
      assert_raises exn fn
    in
    "Recursively Create Directory" >::: [
      "Create"           >:: test_valid;
      "Partially Exists" >:: test_partial;
      "Not a Directory"  >:: test_not_dir;
      "With Exception"   >:: test_exn
    ]
  in
  let test_in_dir =
    let tmpdir = Filename.get_temp_dir_name () in

    let test_normal ctxt =
      let _ = Build.mkdir_p tmpdir in
      let fn _ =
        let actual = Sys.getcwd () in

        (* TODO: Fix the "/private" and trailing "/" problem for real. *)
        let printer x = x in
        let expected = String.concat "" ["/private"; tmpdir] in
        let actual = String.concat "" [actual; "/"] in

        assert_equal ~ctxt ~printer expected actual
      in

      Build.in_dir tmpdir fn ();
      assert_equal ~ctxt cwd (Sys.getcwd ())
    in
    let test_exn ctxt =
      let exn = Failure "foo" in
      let fn _ =
        let fn _ = raise exn in
        Build.in_dir tmpdir fn ()
      in

      assert_raises exn fn;
      assert_equal ~ctxt cwd (Sys.getcwd ())
    in
    "Execute in Directory" >::: [
      "Normal Flow Control" >:: test_normal;
      "Exception"           >:: test_exn
    ]
  in
  let test_rm_rf =
    let test _ =
      let tmpdir = Filename.get_temp_dir_name () in
      let root = Build.mkpath [tmpdir; "root"] in

      let foo = Build.mkpath [root; "foo"] in
      let foo_bar = Build.mkpath [foo; "bar"] in
      let baz = Build.mkpath [root; "baz"] in
      let baz_quux = Build.mkpath [baz; "quux"] in

      let _ = Build.mkdir_p foo_bar in
      let _ = Build.mkdir_p baz_quux in

      let fn (path, n) =
        let rec fn' = function
          | 0 -> ()
          | n ->
            let name =
              Build.mkpath [path; "file" ^ string_of_int n ^ ".txt"]
            in
            let oc = open_out name in
            close_out oc;
            fn' (n - 1)
        in
        fn' n
      in
      List.iter fn [(foo, 1); (foo_bar, 2); (baz, 3); (baz_quux, 4)] ;

      Build.rm_rf root;

      let msg = sprintf "%s: No such file or directory" root in
      try
        let _ = Sys.is_directory root in
        assert_failure (sprintf "Directory %S should not exist" root)
      with
        | Sys_error msg' when msg = msg' -> ()
        | exn -> raise exn
    in
    "Recursively Remove Directory" >:: test
  in
  let test_in_temp_dir =
    let test_normal ctxt =
      let fn _ =
        let actual = Sys.getcwd () in
        let cmp expected actual = expected <> actual in
        let printer x = x in
        assert_equal ~ctxt ~cmp ~printer cwd actual
      in

      Build.in_temp_dir fn ();
      assert_equal ~ctxt cwd (Sys.getcwd ())
    in
    let test_exn ctxt =
      let exn = Failure "foo" in
      let fn _ =
        let fn _ = raise exn in
        Build.in_temp_dir fn ()
      in

      assert_raises exn fn;
      assert_equal ~ctxt cwd (Sys.getcwd ())
    in
    "Execute in Temporary Directory" >::: [
      "Normal Flow Control" >:: test_normal;
      "Exception"           >:: test_exn
    ]
  in
  "Filesystem Helpers" >::: [
    test_mkpath;
    test_mkdir_p;
    test_in_dir;
    test_rm_rf;
    test_in_temp_dir
  ]

(* Contexts *)
let test_ctx =
  let test_empty_ctx ctxt =
    let ctx = Build.empty_ctx in

    assert_equal ~ctxt "" ctx.Build.clang;
    assert_equal ~ctxt "" ctx.stdlib;
    assert_equal ~ctxt "" ctx.home;
    assert_equal ~ctxt "" ctx.root
  in
  let test_ctx_with_clang ctxt =
    let clang_exe = Filename.temp_file "" "" in
    Unix.chmod clang_exe 0o755;

    let ctx = Build.empty_ctx |> Build.ctx_with_clang clang_exe in
    assert_equal ~ctxt clang_exe ctx.Build.clang;

    Sys.remove clang_exe
  in
  let test_ctx_with_stdlib ctxt =
    let ctx = Build.empty_ctx |> Build.ctx_with_stdlib cwd in
    assert_equal ~ctxt cwd ctx.Build.stdlib
  in
  let test_ctx_with_home ctxt =
    let ctx = Build.empty_ctx |> Build.ctx_with_home cwd in
    assert_equal ~ctxt cwd ctx.Build.home
  in
  let test_ctx_with_root ctxt =
    let ctx = Build.empty_ctx |> Build.ctx_with_root cwd in
    assert_equal ~ctxt cwd ctx.Build.root
  in
  let test_ctx_from_env ctxt =
    (* Create mock /usr/bin and /usr/local/bin and the ${PATH} environment variable. *)
    let usr_bin = Build.mkdir_p (Build.mkpath [cwd; "usr"; "bin"]) in
    let usr_local_bin =
      Build.mkdir_p (Build.mkpath [cwd; "usr"; "local"; "bin"])
    in
    let path = String.concat ":" [usr_bin; usr_local_bin] in

    (* Create a mock clang executable in /usr/local/bin *)
    let clang_exe = Build.mkpath [usr_local_bin; "clang"] in
    let oc = open_out clang_exe in
    close_out oc;
    Unix.chmod clang_exe 0o755;

    (* Create a mock stdlib dir *)
    let stdlib_dir = Build.mkdir_p (Build.mkpath [cwd; "stdlib"]) in

    (* Create a mock home directory *)
    let home_dir = Build.mkdir_p (Build.mkpath [cwd; "home"; "username"]) in

    (* Create a mock project *)
    let project_dir =
      Build.mkdir_p (Build.mkpath [home_dir; "projects"; "test-project"])
    in
    let project_file = Build.mkpath [project_dir; "project.json"] in
    let oc = open_out project_file in
    close_out oc;

    (* The test proper *)
    let test ctxt =
      let ctx = Build.empty_ctx |> Build.ctx_from_env in
      assert_equal ~ctxt clang_exe   ctx.Build.clang;
      assert_equal ~ctxt home_dir    ctx.home;
      assert_equal ~ctxt stdlib_dir  ctx.stdlib;
      assert_equal ~ctxt project_dir ctx.root
    in

    (* Set some temporary environment variables and CWD *)
    let test = test
            |> with_env "PATH" path
            |> with_env "HOME" home_dir
            |> with_env "CFNROOT" stdlib_dir
            |> Build.in_dir project_dir
    in

    (* Run the test proper *)
    test ctxt
  in
  "Context" >::: [
    "Empty"                           >:: test_empty_ctx;
    "With Clang Executable"           >:: test_ctx_with_clang;
    "With Standard Library Directory" >:: test_ctx_with_stdlib;
    "With Home Directory"             >:: test_ctx_with_home;
    "With Project Root"               >:: test_ctx_with_root;
    "From Environment"                >:: (test_ctx_from_env |> Build.in_temp_dir)
  ]

(* Test Suite *)
let suite =
  "Build" >::: [
    test_test_init;
    test_test_helpers;
    test_fs_helpers;
    test_ctx
  ]
