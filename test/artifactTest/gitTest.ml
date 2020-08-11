open Format

open Artifact

open OUnit2

let init ?bare:(bare = false) ?branch:(branch = "master") dir =
  let exe = Os.which "git" in
  let args =
    let args = ["-b"; branch; dir] in
    let args = if bare then "--bare" :: args else args in
    args
      |> List.cons "init"
      |> List.cons "git"
      |> Array.of_list
  in
  let proc = Unix.open_process_args exe args in
  let pid = Unix.process_pid proc in
  let _ = Unix.waitpid [] pid in
  Git.repo dir branch

let add_all_and_commit ?message:(message = "") repo =
  Git.git repo ["add"; "--all"] Git.ignore;
  Git.git repo ["commit"; "--allow-empty-message"; "-m"; message] Git.ignore

let add_remote name uri repo =
  Git.git repo ["remote"; "add"; name; Uri.to_string uri] Git.ignore

let current_branch repo =
  Git.git repo ["branch"; "--show-current"] Git.line

let create_branch ?start_point:(start_point = "HEAD") branch_name repo =
  Git.git repo ["branch"; branch_name; start_point] Git.ignore

let checkout refname repo =
  Git.git repo ["checkout"; refname] Git.ignore

let sha repo gitref =
  Git.git repo ["rev-parse"; gitref] Git.line

let timestamp_and_sha repo gitref =
  let line = Git.git repo ["show"; "--format=%ad %H"; "--date=format:%Y%m%d%H%M%S"; gitref] Git.first_line in
  match String.split_on_char ' ' line with
    | sha :: timestamp :: [] -> (sha, timestamp)
    | _ ->
      let msg = sprintf "expected \"<sha> <timestamp>\", found \"%s\"" line in
      let exn = Invalid_argument msg in
      raise exn

let create_tag tag repo =
  Git.git repo ["tag"; tag] Git.lines

let test_repo =
  let root = "/test/dir" in
  let default_branch = "default-branch" in
  let repo = Git.repo root default_branch in

  let test_root ctxt =
    let actual = Git.root repo in
    assert_equal ~ctxt root actual
  in
  let test_default_branch ctxt =
    let actual = Git.default_branch repo in
    assert_equal ~ctxt default_branch actual
  in
  "Repository" >::: [
    "Root"           >:: test_root;
    "Default Branch" >:: test_default_branch
  ]

let test_command =
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

        let actual = Git.lines bs in
        assert_lines ~ctxt actual
      in
      let test_trim_leading ctxt =
        let output = "\n\n" ^ line_one ^ "\n\n" ^ line_two in
        let bs = Bytes.of_string output in

        let actual = Git.lines bs in
        assert_lines ~ctxt actual
      in
      let test_trim_trailing ctxt =
        let output = line_one ^ "\n\n" ^ line_two ^ "\n\n" in
        let bs = Bytes.of_string output in

        let actual = Git.lines bs in
        assert_lines ~ctxt actual
      in
      let test_trim ctxt =
        let output = "\n\n" ^ line_one ^ "\n\n" ^ line_two ^ "\n\n" in
        let bs = Bytes.of_string output in

        let actual = Git.lines bs in
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

        let actual = Git.first_line bs in
        assert_equal ~ctxt line_one actual
      in
      let test_single_line ctxt =
        let output = "\n\n" ^ line_one ^ "\n\n" in
        let bs = Bytes.of_string output in

        let actual = Git.first_line bs in
        assert_equal ~ctxt line_one actual
      in
      let test_blank _ =
        let output = "\n\n\n" in
        let bs = Bytes.of_string output in

        let fn _ = Git.first_line bs in
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

        let actual = Git.line bs in
        assert_equal ~ctxt line_one actual
      in
      let test_invalid _ =
        let output = line_one ^ "\n\n" ^ line_two in
        let bs = Bytes.of_string output in

        let fn _ = Git.line bs in
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
      let actual = Git.ignore bs in
      assert_equal ~ctxt expected actual
    in
    "Output Handlers" >::: [
      test_lines;
      test_first_line;
      test_line;
      "Ignore" >:: test_ignore
    ]
  in
  let test_git =
    let cwd = Sys.getcwd () in
    let repo = Git.repo cwd "" in

    let test_valid _ =
      match Git.git repo ["--help"] Git.lines with
        | [] -> assert_failure "expected output"
        | _ -> ()
    in
    let test_invalid _ =
      let fn _ =
        Git.git repo ["invalid-command"] Git.ignore
      in
      let exn = Failure "exited with status 1" in
      assert_raises exn fn
    in
    "Execute" >::: [
      "Valid"   >:: test_valid;
      "Invalid" >:: test_invalid
    ]
  in
  "Command" >::: [
    test_output;
    test_git
  ]

let test_clone =
  let test_success =
    let filename = "test.txt" in
    let contents = "the contents" in
    let message = "the commit message" in

    let write oc =
      output_string oc contents;
      close_out oc
    in
    let read ic =
      let res = input_line ic in
      close_in ic;
      res
    in

    let test_master ctxt =
      let cwd = Sys.getcwd () in

      let upstream_dir = Filename.concat cwd "upstream_repo" in
      let cloned_dir = Filename.concat cwd "cloned_repo" in

      let upstream_repo = init upstream_dir in

      let path = Filename.concat upstream_dir filename in
      Os.write write 0o644 path;
      add_all_and_commit ~message upstream_repo;

      let upstream_sha = sha upstream_repo "HEAD" in

      let uri = Uri.make ~path:upstream_dir () in
      let cloned_repo = Git.clone uri cloned_dir in
      let cloned_sha = sha cloned_repo "HEAD" in
      let cloned_path = Filename.concat cloned_dir filename in

      assert_equal ~ctxt upstream_sha cloned_sha;

      let msg = sprintf "expected file %s to exist" cloned_path in
      let actual = Sys.file_exists cloned_path in
      assert_bool msg actual;

      let actual = Os.read read cloned_path in
      assert_equal ~ctxt contents actual
    in
    let test_non_master ctxt =
      let branch = "default-branch" in

      let cwd = Sys.getcwd () in

      let upstream_dir = Filename.concat cwd "upstream_repo" in
      let cloned_dir = Filename.concat cwd "cloned_repo" in

      let upstream_repo = init ~branch upstream_dir in

      let path = Filename.concat upstream_dir filename in
      Os.write write 0o644 path;
      add_all_and_commit ~message upstream_repo;

      let upstream_sha = sha upstream_repo "HEAD" in

      let uri = Uri.make ~path:upstream_dir () in
      let cloned_repo = Git.clone uri cloned_dir in
      let cloned_sha = sha cloned_repo "HEAD" in
      let cloned_path = Filename.concat cloned_dir filename in
      let cloned_branch = current_branch cloned_repo in

      assert_equal ~ctxt branch cloned_branch;
      assert_equal ~ctxt upstream_sha cloned_sha;

      let msg = sprintf "expected file %s to exist" cloned_path in
      let actual = Sys.file_exists cloned_path in
      assert_bool msg actual;

      let actual = Os.read read cloned_path in
      assert_equal ~ctxt contents actual
    in
    "Success" >::: [
      "Master"     >:: Os.in_temp_dir test_master;
      "Non-master" >:: Os.in_temp_dir test_non_master
    ]
  in
  let test_failure =
    let test_no_such_repo _ =
      let cwd = Sys.getcwd () in

      let cloned_dir = Filename.concat cwd "cloned_repo" in

      let uri = Uri.of_string "https://example.com/non/existent/repo.git" in
      let fn _ = Git.clone uri cloned_dir in
      let exn = Failure "exited with status 128" in
      assert_raises exn fn
    in
    "Failure" >::: [
      "No such repo" >:: Os.in_temp_dir test_no_such_repo
    ]
  in
  "Clone" >::: [
    test_success;
    test_failure
  ]

let test_fetch =
  let filename = "test.txt" in

  let write contents oc =
    output_string oc contents;
    close_out oc
  in

  let assert_remote ~ctxt remote_repo local_repo remote branch =
    let expected = sha remote_repo ("refs/heads/" ^ branch) in
    let actual = sha local_repo ("refs/remotes/" ^ remote ^ "/" ^ branch) in
    assert_equal ~ctxt expected actual
  in

  let test_single_remote ctxt =
    let cwd = Sys.getcwd () in

    let origin_name = "origin" in
    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_uri = Uri.make ~path:origin_dir () in
    let origin_repo = init origin_dir in

    let path = Filename.concat origin_dir filename in
    Os.write (write "foo") 0o644 path;
    add_all_and_commit origin_repo;

    let cloned_dir = Filename.concat cwd "cloned_repo" in
    let cloned_repo = Git.clone origin_uri cloned_dir in
    assert_remote ~ctxt origin_repo cloned_repo origin_name "master";

    Os.overwrite (write "bar") 0o644 path;
    add_all_and_commit origin_repo;

    let _ = Git.fetch cloned_repo in
    assert_remote ~ctxt origin_repo cloned_repo origin_name "master"
  in
  let test_multiple_remotes ctxt =
    let cwd = Sys.getcwd () in

    let origin_name = "origin" in
    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_repo = init origin_dir in
    let origin_uri = Uri.make ~path:origin_dir () in

    let path = Filename.concat origin_dir filename in
    Os.write (write "foo") 0o644 path;
    add_all_and_commit origin_repo;

    let upstream_one_name = "upstream-one" in
    let upstream_one_dir = Filename.concat cwd "upstream_one_repo" in
    let upstream_one_repo = Git.clone origin_uri upstream_one_dir in
    let upstream_one_uri = Uri.make ~path:upstream_one_dir () in

    let path = Filename.concat upstream_one_dir filename in
    Os.overwrite (write "bar") 0o644 path;
    add_all_and_commit upstream_one_repo;

    let upstream_two_name = "upstream-two" in
    let upstream_two_dir = Filename.concat cwd "upstream_two_repo" in
    let upstream_two_repo = Git.clone origin_uri upstream_two_dir in
    let upstream_two_uri = Uri.make ~path:upstream_two_dir () in

    let path = Filename.concat upstream_two_dir filename in
    Os.overwrite (write "baz") 0o644 path;
    add_all_and_commit upstream_two_repo;

    let cloned_dir = Filename.concat cwd "cloned_repo" in
    let cloned_repo = Git.clone origin_uri cloned_dir in
    let _ = add_remote upstream_one_name upstream_one_uri cloned_repo in
    let _ = add_remote upstream_two_name upstream_two_uri cloned_repo in

    let path = Filename.concat origin_dir filename in
    Os.overwrite (write "quux") 0o644 path;
    add_all_and_commit origin_repo;

    let _ = Git.fetch cloned_repo in
    assert_remote ~ctxt origin_repo cloned_repo origin_name "master";
    assert_remote ~ctxt upstream_one_repo cloned_repo upstream_one_name "master";
    assert_remote ~ctxt upstream_two_repo cloned_repo upstream_two_name "master"
  in
  "Fetch" >::: [
    "Single Remote"    >:: Os.in_temp_dir test_single_remote;
    "Multiple Remotes" >:: Os.in_temp_dir test_multiple_remotes
  ]

let test_checkout =
  let filename = "test.txt" in

  let write contents oc =
    output_string oc contents;
    close_out oc
  in

  let test_valid ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_uri = Uri.make ~path:origin_dir () in
    let origin_repo = init origin_dir in

    let path = Filename.concat origin_dir filename in
    Os.write (write "foo") 0o644 path;
    add_all_and_commit origin_repo;
    let sha_one = sha origin_repo "HEAD" in

    Os.overwrite (write "bar") 0o644 path;
    add_all_and_commit origin_repo;
    let sha_two = sha origin_repo "HEAD" in

    let cloned_dir = Filename.concat cwd "cloned_repo" in
    let cloned_repo = Git.clone origin_uri cloned_dir in

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt sha_two actual;

    let _ = Git.checkout cloned_repo sha_one in

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt sha_one actual
  in
  let test_invalid ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_uri = Uri.make ~path:origin_dir () in
    let origin_repo = init origin_dir in

    let path = Filename.concat origin_dir filename in
    Os.write (write "foo") 0o644 path;
    add_all_and_commit origin_repo;

    Os.overwrite (write "bar") 0o644 path;
    add_all_and_commit origin_repo;
    let sha_two = sha origin_repo "HEAD" in

    let cloned_dir = Filename.concat cwd "cloned_repo" in
    let cloned_repo = Git.clone origin_uri cloned_dir in

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt sha_two actual;

    let fn _ = Git.checkout cloned_repo "invalid-ref" in
    let exn = Failure "exited with status 1" in
    assert_raises exn fn;

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt sha_two actual
  in
  "Checkout" >::: [
    "Valid"   >:: Os.in_temp_dir test_valid;
    "Invalid" >:: Os.in_temp_dir test_invalid
  ]

let test_versions =
  (* let origin_name = "origin" in *)
  let filename = "test.txt" in

  let write contents oc =
    output_string oc contents;
    close_out oc
  in

  let test_bare ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    (* let origin_uri = Uri.make ~path:origin_dir () in *)
    let origin_repo = init origin_dir in

    let path = Filename.concat origin_dir filename in
    Os.overwrite (write "foo") 0o644 path;
    add_all_and_commit origin_repo;
    let (origin_timestamp, origin_sha) = timestamp_and_sha origin_repo "HEAD" in

    let cg = Git.versions origin_repo in

    let fn _ = Semver.latest 0 cg in
    let exn = Not_found in
    assert_raises exn fn;

    let semver = Semver.semver 0 0 0 [origin_timestamp; origin_sha] [] in
    let actual = Semver.latest_prerelease 0 cg in
    assert_equal ~ctxt semver actual;

    let actual = Semver.find semver cg in
    assert_equal ~ctxt origin_sha actual
  in
  let test_v0_branch ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    (* let origin_uri = Uri.make ~path:origin_dir () in *)
    let origin_repo = init origin_dir in

    let path = Filename.concat origin_dir filename in
    Os.overwrite (write "foo") 0o644 path;
    add_all_and_commit origin_repo;
    let (default_timestamp, default_sha) = timestamp_and_sha origin_repo "HEAD" in
    let default_semver = Semver.semver 0 0 0 [default_timestamp; default_sha] [] in

    create_branch "v0" origin_repo;
    checkout "v0" origin_repo;
    Os.overwrite (write "bar") 0o644 path;
    add_all_and_commit origin_repo;
    let (v0_timestamp, v0_sha) = timestamp_and_sha origin_repo "HEAD" in
    let v0_semver = Semver.semver 0 0 0 [v0_timestamp; v0_sha] [] in

    let cg = Git.versions origin_repo in

    let fn _ = Semver.latest 0 cg in
    let exn = Not_found in
    assert_raises exn fn;

    let fn _ = Semver.find default_semver cg in
    let exn = Not_found in
    assert_raises exn fn;

    let actual = Semver.latest_prerelease 0 cg in
    assert_equal ~ctxt v0_semver actual;

    let actual = Semver.find v0_semver cg in
    assert_equal ~ctxt v0_sha actual
  in
  let test_version_branches ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_repo = init origin_dir in

    let path = Filename.concat origin_dir filename in
    Os.overwrite (write "foo") 0o644 path;
    add_all_and_commit origin_repo;

    let v1_branch_name = "v1" in
    create_branch ~start_point:"master" v1_branch_name origin_repo;
    checkout v1_branch_name origin_repo;
    Os.overwrite (write "bar") 0o644 path;
    add_all_and_commit origin_repo;
    let (v1_timestamp, v1_sha) = timestamp_and_sha origin_repo v1_branch_name in
    let v1_semver = Semver.semver 1 0 0 [v1_timestamp; v1_sha] [] in

    let v2_branch_name = "v2" in
    create_branch ~start_point:"master" v2_branch_name origin_repo;
    checkout v2_branch_name origin_repo;
    Os.overwrite (write "baz") 0o644 path;
    add_all_and_commit origin_repo;
    let (v2_timestamp, v2_sha) = timestamp_and_sha origin_repo v2_branch_name in
    let v2_semver = Semver.semver 2 0 0 [v2_timestamp; v2_sha] [] in

    let versions = Git.versions origin_repo in

    let actual = Semver.latest_prerelease 1 versions in
    assert_equal ~ctxt v1_semver actual;

    let actual = Semver.find v1_semver versions in
    assert_equal ~ctxt v1_sha actual;

    let actual = Semver.latest_prerelease 2 versions in
    assert_equal ~ctxt v2_semver actual;

    let actual = Semver.find v2_semver versions in
    assert_equal ~ctxt v2_sha actual
  in
  let test_remote_version_branches ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_uri = Uri.make ~path:origin_dir () in
    let origin_repo = init origin_dir in

    let origin_path = Filename.concat origin_dir filename in
    Os.overwrite (write "foo") 0o644 origin_path;
    add_all_and_commit origin_repo;

    let v1_branch_name = "v1" in
    create_branch ~start_point:"master" v1_branch_name origin_repo;
    checkout v1_branch_name origin_repo;
    Os.overwrite (write "bar") 0o644 origin_path;
    add_all_and_commit origin_repo;
    let (v1_timestamp, v1_sha) = timestamp_and_sha origin_repo v1_branch_name in
    let v1_semver = Semver.semver 1 0 0 [v1_timestamp; v1_sha] [] in

    let v2_branch_name = "v2" in
    create_branch ~start_point:"master" v2_branch_name origin_repo;
    checkout v2_branch_name origin_repo;
    Os.overwrite (write "baz") 0o644 origin_path;
    add_all_and_commit origin_repo;

    checkout "master" origin_repo;

    let other_dir = Filename.concat cwd "other_repo" in
    let other_path = Filename.concat other_dir filename in
    let other_uri = Uri.make ~path:other_dir () in
    let other_repo = Git.clone origin_uri other_dir in

    Unix.sleep 1; (* To get a different timestamp *)

    checkout v2_branch_name other_repo;
    Os.overwrite (write "quux") 0o644 other_path;
    add_all_and_commit other_repo;
    let (v2_timestamp, v2_sha) = timestamp_and_sha other_repo v2_branch_name in
    let v2_semver = Semver.semver 2 0 0 [v2_timestamp; v2_sha] [] in

    let v3_branch_name = "v3" in
    checkout "master" other_repo;
    create_branch v3_branch_name other_repo;
    checkout v3_branch_name other_repo;
    Os.overwrite (write "asdf") 0o644 other_path;
    add_all_and_commit other_repo;
    let (v3_timestamp, v3_sha) = timestamp_and_sha other_repo v3_branch_name in
    let v3_semver = Semver.semver 3 0 0 [v3_timestamp; v3_sha] [] in

    let cloned_dir = Filename.concat cwd "cloned_repo" in
    let cloned_repo = Git.clone origin_uri cloned_dir in
    add_remote "other" other_uri cloned_repo;
    Git.fetch cloned_repo;

    let versions = Git.versions cloned_repo in

    let actual = Semver.latest_prerelease 1 versions in
    assert_equal ~ctxt v1_semver actual;

    let actual = Semver.find v1_semver versions in
    assert_equal ~ctxt v1_sha actual;

    let actual = Semver.latest_prerelease 2 versions in
    assert_equal ~ctxt v2_semver actual;

    let actual = Semver.find v2_semver versions in
    assert_equal ~ctxt v2_sha actual;

    let actual = Semver.latest_prerelease 3 versions in
    assert_equal ~ctxt v3_semver actual;

    let actual = Semver.find v3_semver versions in
    assert_equal ~ctxt v3_sha actual
  in
  let test_tags ctxt =
    let cwd = Sys.getcwd () in

    let origin_dir = Filename.concat cwd "origin_repo" in
    let origin_path = Filename.concat origin_dir filename in
    let origin_uri = Uri.make ~path:origin_dir () in
    let origin_repo = init origin_dir in

    Os.overwrite (write "foo") 0o644 origin_path;
    add_all_and_commit origin_repo;
    let v1_sha = sha origin_repo "HEAD" in
    let v1_semver = Semver.semver 1 2 3 [] [] in
    let _ = create_tag ("v" ^ Semver.to_string v1_semver) origin_repo in

    Os.overwrite (write "bar") 0o644 origin_path;
    add_all_and_commit origin_repo;
    let v2_sha = sha origin_repo "HEAD" in
    let v2_semver = Semver.semver 2 3 4 [] [] in
    let _ = create_tag ("v" ^ Semver.to_string v2_semver) origin_repo in

    Os.overwrite (write "baz") 0o644 origin_path;
    add_all_and_commit origin_repo;
    let v3_sha = sha origin_repo "HEAD" in
    let v3_semver = Semver.semver 3 4 5 [] [] in
    let _ = create_tag ("v" ^ Semver.to_string v3_semver) origin_repo in

    Os.overwrite (write "quux") 0o644 origin_path;
    add_all_and_commit origin_repo;
    let ignored_semver = Semver.semver 4 5 6 [] [] in
    let _ = create_tag (Semver.to_string ignored_semver) origin_repo in

    let cloned_dir = Filename.concat cwd "cloned_repo" in
    let cloned_repo = Git.clone origin_uri cloned_dir in

    let versions = Git.versions cloned_repo in

    let actual = Semver.latest 1 versions in
    assert_equal ~ctxt v1_semver actual;

    let actual = Semver.find v1_semver versions in
    assert_equal ~ctxt v1_sha actual;

    let actual = Semver.latest 2 versions in
    assert_equal ~ctxt v2_semver actual;

    let actual = Semver.find v2_semver versions in
    assert_equal ~ctxt v2_sha actual;

    let actual = Semver.latest 3 versions in
    assert_equal ~ctxt v3_semver actual;

    let actual = Semver.find v3_semver versions in
    assert_equal ~ctxt v3_sha actual;

    let fn _ = Semver.latest 4 versions in
    let exn = Not_found in
    assert_raises exn fn
  in
  "Versions" >::: [
    "Bare Repo"               >:: Os.in_temp_dir test_bare;
    "v0 Branch"               >:: Os.in_temp_dir test_v0_branch;
    "Version Branches"        >:: Os.in_temp_dir test_version_branches;
    "Remote Version Branches" >:: Os.in_temp_dir test_remote_version_branches;
    "Tags"                    >:: Os.in_temp_dir test_tags
  ]

(* Test Suite *)
let suite =
  "Git" >::: [
    test_repo;
    test_command;
    test_clone;
    test_fetch;
    test_checkout;
    test_versions
  ]
