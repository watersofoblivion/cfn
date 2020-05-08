open Format

open OUnit2

open Build

(* Test Initialization *)

let _ = Random.self_init ()

(* Filesystem Helpers *)
let test_mkdir =
  let assert_executable_dir path =
    assert_bool (sprintf "%S is not a directory" path) (Sys.is_directory path);
    let stat = Unix.stat path in
    assert_bool
      (sprintf "%S does not have mode 0o755" path)
      (stat.st_perm = 0o755)
  in

  let test_valid _ =
    let root = Sys.getcwd () in
    let expected = "bar"
                |> Filename.concat "foo"
                |> Filename.concat root
    in

    try
      let _ = Os.mkdir expected in
      assert_executable_dir expected
    with _ ->
      assert_failure ""
  in
  let test_partial _ =
    let root = Sys.getcwd () in
    let expected = "dir_3"
                |> Filename.concat "dir_1"
                |> Filename.concat root
    in
    let partial = "dir_2"
               |> Filename.concat "dir_1"
               |> Filename.concat root
    in

    try
      let _ = Os.mkdir partial in
      let _ = Os.mkdir expected in
      assert_executable_dir expected
    with _ ->
      let msg = sprintf "Expected %S to exist" expected in
      assert_failure msg
  in
  let test_not_dir _ =
    let root = Sys.getcwd () in
    let file_path = Filename.concat root "file" in
    let msg = sprintf "%s is a file" file_path in
    let exn = Invalid_argument msg in

    let fn _ =
      let oc = open_out file_path in
      close_out oc;

      let dir_path = "error"
                  |> Filename.concat "file"
                  |> Filename.concat root
      in
      Os.mkdir dir_path
    in
    assert_raises exn fn
  in
  let test_other_exn _ =
    let path = Filename.concat "/usr" "is-not-world-writable" in
    let exn = Unix.Unix_error (Unix.EPERM, "mkdir", path) in
    let fn _ = Os.mkdir path in
    assert_raises exn fn
  in
  "Recursively Create Directory" >::: [
    "Create"           >:: Os.in_temp_dir test_valid;
    "Partially Exists" >:: Os.in_temp_dir test_partial;
    "Not a Directory"  >:: Os.in_temp_dir test_not_dir;
    "With Exception"   >:: Os.in_temp_dir test_other_exn
  ]

let test_rmdir =
  let test _ =
    let root = Filename.concat (Sys.getcwd ()) "root" in

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
  "Recursively Remove Directory" >:: Os.in_temp_dir test

let test_scan =
  let rec make fn = function
    | 0 -> []
    | n -> (fn n) :: make fn (n - 1)
  in
  let make_files =
    let fn n =
      let filename = "file" ^ string_of_int n ^ ".txt" in
      let oc = open_out filename in
      close_out oc;
      filename
    in
    make fn
  in
  let make_dirs =
    let fn n =
      let dirname = "dir" ^ string_of_int n in
      Unix.mkdir dirname 0o755;
      dirname
    in
    make fn
  in
  let assert_lists_equal expected actual =
    let fn actual =
      let fn expected = actual = expected in
      List.exists fn expected
    in
    if List.for_all fn actual
    then ()
    else
      let expected = String.concat ", " expected in
      let actual = String.concat ", " actual in
      let msg = sprintf "expected [%s] to equal [%s]" expected actual in
      assert_failure msg
  in

  let test_files _ =
    let root = Sys.getcwd () in

    let fn dir =
      let _ = Os.in_dir dir make_files 3 in
      ()
    in
    let _ = make_dirs 3 |> List.iter fn in

    let expected = make_files 3 in
    let actual = Os.files root in

    assert_lists_equal expected actual
  in
  let test_subdirs _ =
    let root = Sys.getcwd () in
    let _ = make_files 3 in

    let expected = make_dirs 3 in
    let actual = Os.subdirs root in

    assert_lists_equal expected actual
  in
  "Directory Scanning" >::: [
    "Files"          >:: Os.in_temp_dir test_files;
    "Subdirectories" >:: Os.in_temp_dir test_subdirs
  ]

let test_in_dir =
  let test_normal ctxt =
    let root = Sys.getcwd () in
    let expected = Filename.concat root "test-dir" in
    Os.mkdir expected;

    let fn _ =
      let actual = Sys.getcwd () in
      assert_equal ~ctxt expected actual
    in

    Os.in_dir expected fn ();

    let cwd = Sys.getcwd () in
    assert_equal ~ctxt root cwd
  in
  let test_exn ctxt =
    let root = Sys.getcwd () in
    let expected = Filename.concat root "test-dir" in
    Os.mkdir expected;

    let exn = Failure "foo" in
    let fn _ =
      let fn _ = raise exn in
      Os.in_dir expected fn ()
    in

    assert_raises exn fn;

    let cwd = Sys.getcwd () in
    assert_equal ~ctxt root cwd
  in
  "Execute in Directory" >::: [
    "Normal Flow Control" >:: Os.in_temp_dir test_normal;
    "Exception"           >:: Os.in_temp_dir test_exn
  ]

let test_temp_dir =
  let test_temp_dir ctxt =
    let temp_dir = Filename.get_temp_dir_name () in
    let assert_temp_dir dir =
      let actual = Filename.dirname dir in
      assert_equal ~ctxt temp_dir (actual ^ Filename.dir_sep);
    in

    let dir = Os.temp_dir () in
    assert_temp_dir dir;

    let other = Os.temp_dir () in
    assert_temp_dir other;

    let cmp x y = not (x = y) in
    assert_equal ~ctxt ~cmp dir other
  in
  "Temporary Directory" >:: Os.in_temp_dir test_temp_dir

let test_in_temp_dir =
  let test_normal ctxt =
    let root = Sys.getcwd () in

    let fn _ =
      let actual = Sys.getcwd () in
      let cmp expected actual = expected <> actual in
      let printer x = x in
      assert_equal ~ctxt ~cmp ~printer root actual
    in

    Os.in_temp_dir fn ();

    let cwd = Sys.getcwd () in
    assert_equal ~ctxt root cwd
  in
  let test_exn ctxt =
    let root = Sys.getcwd () in

    let exn = Failure "foo" in
    let fn _ =
      let fn _ = raise exn in
      Os.in_temp_dir fn ()
    in

    assert_raises exn fn;

    let cwd = Sys.getcwd () in
    assert_equal ~ctxt root cwd
  in
  "Execute in Temporary Directory" >::: [
    "Normal Flow Control" >:: Os.in_temp_dir test_normal;
    "Exception"           >:: Os.in_temp_dir test_exn
  ]

let test_find_in_path =
  "Find in Path" >::: [
  ]

let test_which =
  let test_found ctxt =
    let root = Sys.getcwd () in

    let usr_bin = "bin"
               |> Filename.concat "usr"
               |> Filename.concat root
    in
    let usr_local_bin = "bin"
                     |> Filename.concat "local"
                     |> Filename.concat "usr"
                     |> Filename.concat root
    in
    Os.mkdir usr_bin;
    Os.mkdir usr_local_bin;

    let touch_exe path =
      let oc = open_out path in
      close_out oc;
      Unix.chmod path 0o755
    in

    let foo = "foo" in
    let foo_path = Filename.concat usr_bin foo in
    touch_exe foo_path;

    let bar = "bar" in
    let bar_path = Filename.concat usr_local_bin bar in
    touch_exe bar_path;

    let fn _ =
      let actual = Os.which foo in
      assert_equal ~ctxt foo_path actual;

      let actual = Os.which bar in
      assert_equal ~ctxt bar_path actual
    in

    let path = String.concat ":" [usr_bin; usr_local_bin] in
    TestUtil.with_env "PATH" path fn ()
  in
  let test_not_found _ =
    let non_existent = "i-dont-exist" in

    let msg = sprintf "%S not found in ${PATH}" non_existent in
    let exn = Invalid_argument msg in

    let fn _ = Os.which non_existent in

    assert_raises exn fn
  in
  "Which" >::: [
    "Found"     >:: Os.in_temp_dir test_found;
    "Not Found" >:: Os.in_temp_dir test_not_found
  ]

(* Test Suite *)
let suite =
  "Operating System" >::: [
    test_mkdir;
    test_rmdir;
    test_scan;
    test_in_dir;
    test_temp_dir;
    test_in_temp_dir;
    test_find_in_path;
    test_which;
  ]
