open Format

open OUnit2

open System

(* Assertions *)


let assert_output extractor ~ctxt output expected =
  output
    |> extractor
    |> Bytes.to_string
    |> Assert.text_equals ~ctxt expected

let assert_stdout = assert_output Os.stdout
let assert_stderr = assert_output Os.stderr

let assert_non_zero ~ctxt ?stdout:(stdout = None) ?stderr:(stderr = None) status fn =
  try
    fn ();
    assert_failure "expected Os.NonZero to be raised"
  with
    | Os.NonZero(actual, output) ->
      (* let msg = output |> Os.stdout |> Bytes.to_string in
      Format.eprintf "STDOUT:\n%s\n%!" msg;
      let msg = output |> Os.stderr |> Bytes.to_string in
      Format.eprintf "STDERR:\n%s\n%!" msg; *)
      let _ = match stdout with
        | Some stdout -> assert_stdout ~ctxt output stdout
        | None -> ""
      in
      let _ = match stderr with
        | Some stderr -> assert_stderr ~ctxt output stderr
        | None -> ""
      in
      assert_equal ~ctxt status actual
    | exn -> raise exn

(* Helpers *)

let read_all ic =
  let buflen = 1024 in
  let buf = Buffer.create buflen in
  let bs = Bytes.create buflen in
  let rec read _ = match input ic bs 0 buflen with
    | 0 -> Buffer.to_bytes buf
    | n ->
      let bs' = Bytes.sub bs 0 n in
      Buffer.add_bytes buf bs';
      read ()
  in
  read ()

(* Test Initialization *)

let _ = Random.self_init ()

let test_dir =
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
  in
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
  in
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
    let test_dirs _ =
      let root = Sys.getcwd () in
      let _ = make_files 3 in

      let expected = make_dirs 3 in
      let actual = Os.dirs root in

      assert_lists_equal expected actual
    in
    let test_subdirs _ =
      let root = Sys.getcwd () in

      let created =
        let map dir =
          let map subdir = Filename.concat dir subdir in
          Os.in_dir dir make_dirs 3
            |> List.map map
        in
        let initial = make_dirs 3 in
        let next =
          initial
            |> List.map map
            |> List.flatten
        in
        let final =
          next
            |> List.map map
            |> List.flatten
        in
        final
          |> List.append next
          |> List.append initial
      in

      let found = Os.subdirs root in

      let iter dir =
        let msg = sprintf "expected to find %S" dir in
        found
          |> List.mem dir
          |> assert_bool msg
      in
      List.iter iter created
    in
    "Directory Scanning" >::: [
      "Files"          >:: Os.in_temp_dir test_files;
      "Directories"    >:: Os.in_temp_dir test_dirs;
      "Subdirectories" >:: Os.in_temp_dir test_subdirs
    ]
  in
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
  in
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
  in
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
  in
  "Directory Handling" >::: [
    test_mkdir;
    test_rmdir;
    test_scan;
    test_in_dir;
    test_temp_dir;
    test_in_temp_dir;
  ]

let test_search =
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
      let exn = Not_found in
      let fn _ = Os.which non_existent in
      assert_raises exn fn
    in
    "Which" >::: [
      "Found"     >:: Os.in_temp_dir test_found;
      "Not Found" >:: Os.in_temp_dir test_not_found
    ]
  in
  let test_find_in_path =
    let test_found ctxt =
      let cwd = Sys.getcwd () in
      let foo_dir = Filename.concat cwd "foo" in
      let bar_dir = Filename.concat foo_dir "bar" in

      let filename = "filename" in
      let expected = Filename.concat foo_dir filename in

      Os.mkdir foo_dir;
      Os.mkdir bar_dir;
      let oc = open_out expected in
      close_out oc;

      let actual = Os.find_in_path filename bar_dir in
      assert_equal ~ctxt expected actual
    in
    let test_not_found _ =
      let cwd = Sys.getcwd () in
      let non_existent = "i-dont-exist" in
      let exn = Not_found in
      let fn _ = Os.find_in_path non_existent cwd in
      assert_raises exn fn
    in
    "Find in Path" >::: [
      "Found"     >:: Os.in_temp_dir test_found;
      "Not Found" >:: Os.in_temp_dir test_not_found
    ]
  in
  "Filesystem Searching" >::: [
    test_which;
    test_find_in_path;
  ]

let test_atomic =
  let contents = Bytes.of_string "The file contents" in
  let write_all data oc =
    let len = Bytes.length data in
    output oc data 0 len
  in

  let test_read =
    let write_data_file _ =
      let (filename, oc) = Filename.open_temp_file "" "" in
      let finally _ = close_out oc in

      let fn _ =
        write_all contents oc;
        filename
      in

      Fun.protect ~finally fn
    in

    let test_success ctxt =
      let filename = write_data_file () in
      let actual = Os.read read_all filename in
      assert_equal ~ctxt ~cmp:Bytes.equal contents actual
    in
    let test_not_found _ =
      let non_existent = "i-dont-exist" in

      let msg = sprintf "%s: No such file or directory" non_existent in
      let exn = Sys_error msg in

      let fn _ = Os.read read_all "i-dont-exist" in
      assert_raises exn fn
    in
    let test_exception _ =
      let filename = write_data_file () in
      let exn = Failure "Bang!" in
      let fn _ = raise exn in
      let fn _ = Os.read fn filename in
      assert_raises exn fn
    in
    "Read" >::: [
      "Success"        >:: Os.in_temp_dir test_success;
      "File Not Found" >:: Os.in_temp_dir test_not_found;
      "Exception"      >:: Os.in_temp_dir test_exception;
    ]
  in
  let test_write =
    let test_success ctxt =
      let cwd = Sys.getcwd () in

      let filename = "foo" in
      let path = Filename.concat cwd filename in
      let mode = 0o644 in

      let fn = write_all contents in
      Os.write fn mode path;

      let msg = sprintf "file %S does not exist" path in
      let actual = Sys.file_exists path in
      assert_bool msg actual;

      let stat = Unix.stat path in
      let actual = stat.st_perm in
      assert_equal ~ctxt mode actual;

      let ic = open_in path in
      let finally _ = close_in ic in
      let fn _ =
        let actual = read_all ic in
        let printer x = sprintf "%S" (Bytes.to_string x) in
        assert_equal ~ctxt ~printer ~cmp:Bytes.equal contents actual
      in
      Fun.protect ~finally fn
    in
    let test_exists _ =
      let cwd = Sys.getcwd () in

      let filename = "foo" in
      let path = Filename.concat cwd filename in
      let mode = 0o644 in

      let fn = write_all contents in
      Os.write fn mode path;

      let msg = sprintf "%s: File exists" path in
      let exn = Sys_error msg in
      let fn _ = Os.write fn mode path in
      assert_raises exn fn
    in
    let test_exception _ =
      let cwd = Sys.getcwd () in

      let filename = "foo" in
      let path = Filename.concat cwd filename in
      let mode = 0o644 in

      let exn = Failure "Bang!" in
      let fn oc =
        write_all contents oc;
        raise exn
      in

      let fn _ = Os.write fn mode path in
      assert_raises exn fn;

      let msg = sprintf "Expected %S not to exist" path in
      let actual = not (Sys.file_exists path) in
      assert_bool msg actual
    in
    "Write" >::: [
      "Success"     >:: Os.in_temp_dir test_success;
      "File Exists" >:: Os.in_temp_dir test_exists;
      "Exception"   >:: Os.in_temp_dir test_exception
    ]
  in
  let test_overwrite =
    let test_create ctxt =
      let cwd = Sys.getcwd () in

      let filename = "foo" in
      let path = Filename.concat cwd filename in
      let mode = 0o644 in

      let fn = write_all contents in
      Os.overwrite fn mode path;

      let msg = sprintf "file %S does not exist" path in
      let actual = Sys.file_exists path in
      assert_bool msg actual;

      let stat = Unix.stat path in
      let actual = stat.st_perm in
      assert_equal ~ctxt mode actual;

      let ic = open_in path in
      let finally _ = close_in ic in
      let fn _ =
        let actual = read_all ic in
        let printer x = sprintf "%S" (Bytes.to_string x) in
        assert_equal ~ctxt ~printer ~cmp:Bytes.equal contents actual
      in
      Fun.protect ~finally fn
    in
    let test_exists ctxt =
      let cwd = Sys.getcwd () in

      let filename = "foo" in
      let path = Filename.concat cwd filename in
      let mode = 0o644 in

      let fn = write_all contents in
      Os.overwrite fn mode path;

      let new_contents = Bytes.of_string "New file contents" in
      let new_mode = 0o755 in
      let fn = write_all new_contents in
      Os.overwrite fn new_mode path;

      let msg = sprintf "file %S does not exist" path in
      let actual = Sys.file_exists path in
      assert_bool msg actual;

      let stat = Unix.stat path in
      let actual = stat.st_perm in
      assert_equal ~ctxt new_mode actual;

      let ic = open_in path in
      let finally _ = close_in ic in
      let fn _ =
        let actual = read_all ic in
        let printer x = sprintf "%S" (Bytes.to_string x) in
        assert_equal ~ctxt ~printer ~cmp:Bytes.equal new_contents actual
      in
      Fun.protect ~finally fn
    in
    let test_exception ctxt =
      let cwd = Sys.getcwd () in

      let filename = "foo" in
      let path = Filename.concat cwd filename in
      let mode = 0o644 in

      let fn = write_all contents in
      Os.overwrite fn mode path;

      let new_contents = Bytes.of_string "New file contents" in
      let new_mode = 0o755 in
      let exn = Failure "Bang!" in
      let fn oc =
        write_all new_contents oc;
        raise exn
      in

      let fn _ = Os.overwrite fn new_mode path in
      assert_raises exn fn;

      let msg = sprintf "file %S does not exist" path in
      let actual = Sys.file_exists path in
      assert_bool msg actual;

      let stat = Unix.stat path in
      let actual = stat.st_perm in
      assert_equal ~ctxt mode actual;

      let ic = open_in path in
      let finally _ = close_in ic in
      let fn _ =
        let actual = read_all ic in
        let printer x = sprintf "%S" (Bytes.to_string x) in
        assert_equal ~ctxt ~printer ~cmp:Bytes.equal contents actual
      in
      Fun.protect ~finally fn
    in
    "Overwrite" >::: [
      "Create"    >:: Os.in_temp_dir test_create;
      "Exists"    >:: Os.in_temp_dir test_exists;
      "Exception" >:: Os.in_temp_dir test_exception
    ]
  in
  "Atomic File Operations" >::: [
    test_read;
    test_write;
    test_overwrite;
  ]

let test_output =
  let line_one = "line 1" in
  let line_two = "line 2" in

  let test_lines =
    let assert_lines ~ctxt actual =
      assert_equal ~ctxt 3 (List.length actual);
      match actual with
        | one :: "" :: two :: [] ->
          assert_equal ~ctxt one line_one;
          assert_equal ~ctxt two line_two
        | _ -> assert_failure "Unpossible!"
    in

    let test_split ctxt =
      let output = line_one ^ "\n\n" ^ line_two in
      let bs = Bytes.of_string output in

      let actual = Os.lines bs in
      assert_lines ~ctxt actual
    in
    let test_trim_leading ctxt =
      let output = "\n\n" ^ line_one ^ "\n\n" ^ line_two in
      let bs = Bytes.of_string output in

      let actual = Os.lines bs in
      assert_lines ~ctxt actual
    in
    let test_trim_trailing ctxt =
      let output = line_one ^ "\n\n" ^ line_two ^ "\n\n" in
      let bs = Bytes.of_string output in

      let actual = Os.lines bs in
      assert_lines ~ctxt actual
    in
    let test_trim ctxt =
      let output = "\n\n" ^ line_one ^ "\n\n" ^ line_two ^ "\n\n" in
      let bs = Bytes.of_string output in

      let actual = Os.lines bs in
      assert_lines ~ctxt actual
    in
    "Lines" >::: [
      "Splits Lines"  >:: test_split;
      "Trim Leading"  >:: test_trim_leading;
      "Trim Trailing" >:: test_trim_trailing;
      "Trim"          >:: test_trim
    ]
  in
  let test_first_line =
    let test_multi_line ctxt =
      let output = "\n\n" ^ line_one ^ "\n\n" ^ line_two ^ "\n\n" in
      let bs = Bytes.of_string output in

      let actual = Os.first_line bs in
      assert_equal ~ctxt line_one actual
    in
    let test_single_line ctxt =
      let output = "\n\n" ^ line_one ^ "\n\n" in
      let bs = Bytes.of_string output in

      let actual = Os.first_line bs in
      assert_equal ~ctxt line_one actual
    in
    let test_blank _ =
      let output = "\n\n\n" in
      let bs = Bytes.of_string output in

      let fn _ = Os.first_line bs in
      let exn = Invalid_argument "no output lines" in
      assert_raises exn fn
    in
    "First Line" >::: [
      "Multi-line"  >:: test_multi_line;
      "Single line" >:: test_single_line;
      "Blank"       >:: test_blank
    ]
  in
  let test_line =
    let test_valid ctxt =
      let output = "\n\n" ^ line_one ^ "\n\n" in
      let bs = Bytes.of_string output in

      let actual = Os.line bs in
      assert_equal ~ctxt line_one actual
    in
    let test_invalid _ =
      let output = line_one ^ "\n\n" ^ line_two in
      let bs = Bytes.of_string output in

      let fn _ = Os.line bs in
      let exn = Invalid_argument "expected 1 line, found 3" in
      assert_raises exn fn
    in
    "Line" >::: [
      "Valid"   >:: test_valid;
      "Invalid" >:: test_invalid
    ]
  in
  let test_ignore ctxt =
    let bs = Bytes.of_string "the-output" in

    let expected = () in
    let actual = Os.ignore bs in
    assert_equal ~ctxt expected actual
  in
  "Output Handlers" >::: [
    test_lines;
    test_first_line;
    test_line;
    "Ignore" >:: test_ignore
  ]

let test_process =
  let test_output =
    let output constr str =
      sprintf "%s" str
        |> Bytes.of_string
        |> constr
    in
    let out_one = "STDOUT 1\n" in
    let out_two = "STDOUT 2\n" in
    let err_one = "STDERR 1\n" in
    let err_two = "STDERR 2\n" in
    let output = [
      output Os.of_stdout out_one;
      output Os.of_stderr err_one;
      output Os.of_stdout out_two;
      output Os.of_stderr err_two
    ] in

    let test_stdout ctxt =
      let expected = Bytes.of_string (out_one ^ out_two) in
      let actual = Os.stdout output in

      assert_equal ~ctxt ~cmp:Bytes.equal expected actual
    in
    let test_stderr ctxt =
      let expected = Bytes.of_string (err_one ^ err_two) in
      let actual = Os.stderr output in

      assert_equal ~ctxt ~cmp:Bytes.equal expected actual
    in
    let test_combined ctxt =
      let expected = Bytes.of_string (out_one ^ err_one ^ out_two ^ err_two) in
      let actual = Os.combined output in

      assert_equal ~ctxt ~cmp:Bytes.equal expected actual
    in
    let test_dump ctxt =
      let temp_dir = Sys.getcwd () in

      let (out_name, out_chan) = Filename.open_temp_file ~temp_dir "" "" in
      let (err_name, err_chan) = Filename.open_temp_file ~temp_dir "" "" in
      let finally _ =
        close_out out_chan;
        close_out err_chan
      in
      let dump _ = Os.dump out_chan err_chan output in
      Fun.protect ~finally dump;

      let expected = Os.stdout output in
      let actual = Os.read read_all out_name in
      assert_equal ~ctxt ~cmp:Bytes.equal expected actual;

      let expected = Os.stderr output in
      let actual = Os.read read_all err_name in
      assert_equal ~ctxt ~cmp:Bytes.equal expected actual
    in
    "Output" >::: [
      "Standard Output" >:: test_stdout;
      "Standard Error"  >:: test_stderr;
      "Combined"        >:: test_combined;
      "Dump"            >:: Os.in_temp_dir test_dump
    ]
  in
  let test_run =
    let test_success =
      let test_output _ = ()
      in
      "Success" >::: [
        "Output" >:: test_output
      ]
    in
    let test_error =
      let test_exit_status ctxt =
        let expected = 1 in
        try
          let _ = Os.run "ls" ["--unknown-option"] Os.ignore in
          let msg = "expected exception" in
          assert_failure msg
        with
          | Os.NonZero(actual, _) ->
            assert_equal ~ctxt expected actual
          | _ ->
            let msg = "expected Os.NonZero" in
            assert_failure msg
      in
      let test_output ctxt =
        try
          let _ = Os.run "ls" ["--unknown-option"] Os.ignore in
          let msg = "expected exception" in
          assert_failure msg
        with
          | Os.NonZero(_, []) ->
            let msg = "expected output" in
            assert_failure msg
          | Os.NonZero(_, output) ->
            let stdout = Os.stdout output in
            let stderr = Os.stderr output in
            let combined = Os.combined output in

            let not_eq x y = not (x = y) in

            assert_equal ~ctxt 0 (Bytes.length stdout);
            assert_equal ~ctxt ~cmp:not_eq 0 (Bytes.length stderr);
            assert_equal ~ctxt ~cmp:Bytes.equal stderr combined;
          | _ ->
            let msg = "expected Os.NonZero" in
            assert_failure msg
      in
      "Error" >::: [
        "Exit Status" >:: test_exit_status;
        "Output"      >:: test_output
      ]
    in
    let test_not_found =
      let test_not_found _ =
        let fn _ = Os.run "i-dont-exist" [] Os.ignore in
        let exn = Not_found in
        assert_raises exn fn
      in
      "Not Found" >::: [
        "Raises exception" >:: test_not_found
      ]
    in
    "Run" >::: [
      test_success;
      test_error;
      test_not_found
    ]
  in
  "Process Control" >::: [
    test_output;
    test_run
  ]

(* Test Suite *)
let suite =
  "Operating System" >::: [
    test_dir;
    test_search;
    test_atomic;
    test_output;
    test_process
  ]
