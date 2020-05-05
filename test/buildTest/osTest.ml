open Format

open OUnit2

open Build

(* Test Initialization *)

let _ = Random.self_init ()

(* Useful globals *)
let cwd = Sys.getcwd ()

(* Filesystem Helpers *)
let test_fs_helpers =
  let test_mkdir =
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
        Os.mkdir path
      in
      let _ = Os.in_dir tmpdir fn () in

      (* TODO: Fix the "/private" and trailing "/" problem for real. *)
      let printer path = path in
      assert_equal ~ctxt ~printer (String.concat "" ["/private"; expected]) tmpdir;

      assert_executable_dir tmpdir;
      assert_executable_dir (Filename.dirname tmpdir)
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
      let _ = Os.mkdir partial in
      let _ = Os.mkdir expected in

      assert_equal ~ctxt expected tmpdir;
      assert_executable_dir tmpdir;
      assert_executable_dir (Filename.dirname tmpdir)
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
        let _ = Os.mkdir dir_path in
        let msg = sprintf "expected %S to fail because %S is a file" dir_path file_path in
        assert_failure msg
      with Failure _ -> ()
    in
    let test_exn _ =
      let path = Filename.concat "/usr" "is-not-world-writable" in
      let exn = Unix.Unix_error (Unix.EPERM, "mkdir", path) in
      let fn _ = Os.mkdir path in
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
      let _ = Os.mkdir tmpdir in
      let fn _ =
        let actual = Sys.getcwd () in

        (* TODO: Fix the "/private" and trailing "/" problem for real. *)
        let printer x = x in
        let expected = String.concat "" ["/private"; tmpdir] in
        let actual = String.concat "" [actual; "/"] in

        assert_equal ~ctxt ~printer expected actual
      in

      Os.in_dir tmpdir fn ();
      assert_equal ~ctxt cwd (Sys.getcwd ())
    in
    let test_exn ctxt =
      let exn = Failure "foo" in
      let fn _ =
        let fn _ = raise exn in
        Os.in_dir tmpdir fn ()
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
      let root = Filename.concat tmpdir "root" in

      let foo = Filename.concat root "foo" in
      let foo_bar = Filename.concat foo "bar" in
      let baz = Filename.concat root "baz" in
      let baz_quux = Filename.concat baz "quux" in

      let _ = Os.mkdir foo_bar in
      let _ = Os.mkdir baz_quux in

      let fn (path, n) =
        let rec fn' = function
          | 0 -> ()
          | n ->
            let name =
              Filename.concat path ("file" ^ string_of_int n ^ ".txt")
            in
            let oc = open_out name in
            close_out oc;
            fn' (n - 1)
        in
        fn' n
      in
      List.iter fn [(foo, 1); (foo_bar, 2); (baz, 3); (baz_quux, 4)] ;

      Os.rmdir root;

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

      Os.in_temp_dir fn ();
      assert_equal ~ctxt cwd (Sys.getcwd ())
    in
    let test_exn ctxt =
      let exn = Failure "foo" in
      let fn _ =
        let fn _ = raise exn in
        Os.in_temp_dir fn ()
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
    test_mkdir;
    test_in_dir;
    test_rm_rf;
    test_in_temp_dir;
  ]

(* Test Suite *)
let suite =
  "Operating System" >::: [
    test_fs_helpers
  ]
