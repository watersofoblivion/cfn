open Format

open OUnit2

open Build

(* Test Initialization *)

let _ = Random.self_init ()

(* Useful globals *)
let cwd = Sys.getcwd ()

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
    test_in_temp_dir;
  ]

(* Test Suite *)
let suite =
  "Operating System" >::: [
    test_fs_helpers
  ]
