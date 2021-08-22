open Format

open System

open OUnit2

(* Helpers *)

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
  Git.git repo ["add"; "--all"] Os.ignore;
  Git.git repo ["commit"; "--allow-empty-message"; "-m"; message] Os.ignore

let add_remote name uri repo =
  Git.git repo ["remote"; "add"; name; Uri.to_string uri] Os.ignore

let current_branch repo =
  Git.git repo ["branch"; "--show-current"] Os.line

let create_branch ?start_point:(start_point = "HEAD") branch_name repo =
  Git.git repo ["branch"; branch_name; start_point] Os.ignore

let checkout refname repo =
  Git.git repo ["checkout"; refname] Os.ignore

let sha repo gitref =
  Git.git repo ["rev-parse"; gitref] Os.line

let timestamp_and_sha repo gitref =
  let line = Git.git repo ["show"; "--format=%ad %H"; "--date=format:%Y%m%d%H%M%S"; gitref] Os.first_line in
  match String.split_on_char ' ' line with
    | sha :: timestamp :: [] -> (sha, timestamp)
    | _ ->
      let msg = sprintf "expected \"<sha> <timestamp>\", found \"%s\"" line in
      let exn = Invalid_argument msg in
      raise exn

let create_tag tag repo =
  Git.git repo ["tag"; tag] Os.lines

let repo_info name =
  let cwd = Sys.getcwd () in

  let dir = Filename.concat cwd (name ^ "_repo") in
  let path = Filename.concat dir "test.txt" in
  let uri = Uri.make ~path:dir () in
  (dir, path, uri)

let make_repo ?branch:(branch = "master") _ =
  let (dir, path, uri) = repo_info "origin" in
  let repo = init ~branch dir in
  (path, uri, repo)

let dup_repo origin_uri name =
  let (dir, path, uri) = repo_info name in
  let repo = Git.clone origin_uri dir in
  (path, uri, repo)

let commit contents path repo =
  let write oc =
    output_string oc contents;
    close_out oc
  in
  Os.overwrite write 0o644 path;
  add_all_and_commit repo

let semver_and_sha major repo gitref =
  let (timestamp, sha) = timestamp_and_sha repo gitref in
  let semver = Semver.semver major 0 0 [timestamp; sha] [] in
  (semver, sha)

let version_branch ?create:(create = true) major contents path repo =
  let branch_name = "v" ^ (string_of_int major) in
  if create then create_branch ~start_point:"master" branch_name repo;
  checkout branch_name repo;
  commit contents path repo;
  semver_and_sha major repo branch_name

let tag ?ignore:(ignore = false) contents path major minor patch repo =
  commit contents path repo;
  let sha = sha repo "HEAD" in
  let semver = Semver.semver major minor patch [] [] in
  let tag =
    let semver_string = Semver.to_string semver in
    if ignore then semver_string else "v" ^ semver_string
  in
  let _ = create_tag tag repo in
  (sha, semver)

(* Assertions *)

(* Tests *)

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
  let cwd = Sys.getcwd () in
  let repo = Git.repo cwd "" in

  let test_valid _ =
    match Git.git repo ["--help"] Os.lines with
      | [] -> assert_failure "expected output"
      | _ -> ()
  in
  let test_invalid ctxt =
    let fn _ = Git.git repo ["invalid-command"] Os.ignore in
    let stderr = Some "git: 'invalid-command' is not a git command. See 'git --help'.\n" in
    OsTest.assert_non_zero ~ctxt ~stderr 1 fn
  in
  "Command" >::: [
    "Valid"   >:: test_valid;
    "Invalid" >:: test_invalid
  ]

let test_clone =
  let test_success =
    let filename = "test.txt" in
    let contents = "the contents" in

    let read ic =
      let res = input_line ic in
      close_in ic;
      res
    in

    let test_master ctxt =
      let (origin_path, origin_uri, origin_repo) = make_repo () in
      let path = Filename.concat origin_path filename in
      commit contents path origin_repo;
      let origin_sha = sha origin_repo "HEAD" in

      let (cloned_path, _, cloned_repo) = dup_repo origin_uri "cloned" in
      let path = Filename.concat cloned_path filename in

      let actual = sha cloned_repo "HEAD" in
      assert_equal ~ctxt origin_sha actual;

      let msg = sprintf "expected file %s to exist" path in
      let actual = Sys.file_exists path in
      assert_bool msg actual;

      let actual = Os.read read path in
      assert_equal ~ctxt contents actual
    in
    let test_non_master ctxt =
      let branch = "default-branch" in

      let (origin_path, origin_uri, origin_repo) = make_repo ~branch () in
      let path = Filename.concat origin_path filename in
      commit contents path origin_repo;
      let origin_sha = sha origin_repo "HEAD" in

      let (cloned_path, _, cloned_repo) = dup_repo origin_uri "cloned" in

      let actual = current_branch cloned_repo in
      assert_equal ~ctxt branch actual;

      let actual = sha cloned_repo "HEAD" in
      assert_equal ~ctxt origin_sha actual;

      let path = Filename.concat cloned_path filename in

      let msg = sprintf "expected file %s to exist" path in
      let actual = Sys.file_exists path in
      assert_bool msg actual;

      let actual = Os.read read path in
      assert_equal ~ctxt contents actual
    in
    "Success" >::: [
      "Master"     >:: Os.in_temp_dir test_master;
      "Non-master" >:: Os.in_temp_dir test_non_master
    ]
  in
  let test_failure =
    let test_no_such_repo ctxt =
      let cwd = Sys.getcwd () in
      let temp_dir = Filename.concat cwd "cloned_repo" in
      let fn _ =
        let uri = Uri.of_string "https://example.com/non/existent/repo.git" in
        let _ = Git.clone uri temp_dir in
        ()
      in
      let stderr =
        let msg =
          sprintf "Cloning into '%s'...\n" temp_dir ^
          "fatal: repository 'https://example.com/non/existent/repo.git/' not found\n"
        in
        Some msg
      in
      OsTest.assert_non_zero ~ctxt ~stderr 128 fn
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

  let assert_remote ~ctxt remote_repo local_repo remote branch =
    let expected = sha remote_repo ("refs/heads/" ^ branch) in
    sha local_repo ("refs/remotes/" ^ remote ^ "/" ^ branch)
      |> Assert.string_equals ~ctxt expected
      |> Assert.success
  in

  let test_single_remote ctxt =
    let origin_name = "origin" in
    let (origin_path, origin_uri, origin_repo) = make_repo () in

    let path = Filename.concat origin_path filename in
    commit "foo" path origin_repo;

    let (_, _, cloned_repo) = dup_repo origin_uri "cloned" in
    assert_remote ~ctxt origin_repo cloned_repo origin_name "master";

    commit "bar" path origin_repo;

    let _ = Git.fetch cloned_repo in
    assert_remote ~ctxt origin_repo cloned_repo origin_name "master"
  in
  let test_multiple_remotes ctxt =
    let origin_name = "origin" in
    let (origin_path, origin_uri, origin_repo) = make_repo () in
    let path = Filename.concat origin_path filename in
    commit "foo" path origin_repo;

    let upstream_one_name = "upstream-one" in
    let (upstream_one_path, upstream_one_uri, upstream_one_repo) = dup_repo origin_uri upstream_one_name in
    let path = Filename.concat upstream_one_path filename in
    commit "bar" path upstream_one_repo;

    let upstream_two_name = "upstream-two" in
    let (upstream_two_path, upstream_two_uri, upstream_two_repo) = dup_repo origin_uri upstream_two_name in
    let path = Filename.concat upstream_two_path filename in
    commit "baz" path upstream_two_repo;

    let (_, _, cloned_repo) = dup_repo origin_uri "cloned" in
    let _ = add_remote upstream_one_name upstream_one_uri cloned_repo in
    let _ = add_remote upstream_two_name upstream_two_uri cloned_repo in

    let path = Filename.concat origin_path filename in
    commit "quux" path origin_repo;

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

  let test_valid ctxt =
    let (origin_path, origin_uri, origin_repo) = make_repo () in
    let path = Filename.concat origin_path filename in

    commit "foo" path origin_repo;
    let sha_one = sha origin_repo "HEAD" in

    commit "bar" path origin_repo;
    let sha_two = sha origin_repo "HEAD" in

    let (_, _, cloned_repo) = dup_repo origin_uri "cloned" in

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt sha_two actual;

    let _ = Git.checkout cloned_repo sha_one in

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt sha_one actual
  in


  let test_invalid ctxt =
    let (origin_path, origin_uri, origin_repo) = make_repo () in
    let path = Filename.concat origin_path filename in
    commit "foo" path origin_repo;
    let head_sha = sha origin_repo "HEAD" in

    let (_, _, cloned_repo) = dup_repo origin_uri "cloned" in

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt head_sha actual;

    let fn _ = Git.checkout cloned_repo "invalid-ref" in
    let stderr = Some "error: pathspec 'invalid-ref' did not match any file(s) known to git\n" in
    OsTest.assert_non_zero ~ctxt ~stderr 1 fn;

    let actual = sha cloned_repo "HEAD" in
    assert_equal ~ctxt head_sha actual
  in
  "Checkout" >::: [
    "Valid"   >:: Os.in_temp_dir test_valid;
    "Invalid" >:: Os.in_temp_dir test_invalid
  ]

let test_versions =
  let assert_version latest ~ctxt semver sha versions =
    let major = Semver.major semver in
    versions
      |> latest major
      |> assert_equal ~ctxt semver;
    versions
      |> Semver.find semver
      |> assert_equal ~ctxt sha
  in
  let assert_latest = assert_version Semver.latest in
  let assert_prerelease = assert_version Semver.latest_prerelease in

  let assert_no_version latest semver versions =
    let major = Semver.major semver in
    let fn _ = latest major versions in
    let exn = Not_found in
    assert_raises exn fn
  in
  let assert_no_latest = assert_no_version Semver.latest in
  (* let assert_no_prerelease = assert_no_version Semver.latest_prerelease in *)

  let test_bare ctxt =
    let (origin_path, _, origin_repo) = make_repo () in

    commit "foo" origin_path origin_repo;
    let (origin_semver, origin_sha) = semver_and_sha 0 origin_repo "HEAD" in

    let versions = Git.versions origin_repo in

    assert_no_latest origin_semver versions;
    assert_prerelease ~ctxt origin_semver origin_sha versions
  in
  let test_v0_branch ctxt =
    let (origin_path, _, origin_repo) = make_repo () in

    commit "foo" origin_path origin_repo;
    let (default_semver, _) = semver_and_sha 0 origin_repo "HEAD" in
    let (v0_semver, v0_sha) = version_branch 0 "bar" origin_path origin_repo in

    let versions = Git.versions origin_repo in
    assert_no_latest default_semver versions;

    (* assert_no_prerelease default_semver versions; *)
    let fn _ = Semver.find default_semver versions in
    let exn = Not_found in
    assert_raises exn fn;

    assert_prerelease ~ctxt v0_semver v0_sha versions
  in
  let test_version_branches ctxt =
    let (origin_path, _, origin_repo) = make_repo () in

    commit "foo" origin_path origin_repo;
    let (v1_semver, v1_sha) = version_branch 1 "bar" origin_path origin_repo in
    let (v2_semver, v2_sha) = version_branch 2 "baz" origin_path origin_repo in

    let versions = Git.versions origin_repo in

    assert_prerelease ~ctxt v1_semver v1_sha versions;
    assert_prerelease ~ctxt v2_semver v2_sha versions
  in
  let test_remote_version_branches ctxt =
    let (origin_path, origin_uri, origin_repo) = make_repo () in

    commit "foo" origin_path origin_repo;
    let (v1_semver, v1_sha) = version_branch 1 "bar" origin_path origin_repo in
    let _ = version_branch 2 "baz" origin_path origin_repo in
    checkout "master" origin_repo;

    Unix.sleep 1; (* To get a different timestamp *)
    let (other_path, other_uri, other_repo) = dup_repo origin_uri "other" in
    let (v2_semver, v2_sha) = version_branch ~create:false 2 "quux" other_path other_repo in
    let (v3_semver, v3_sha) = version_branch 3 "asdf" other_path other_repo in

    let (_, _, cloned_repo) = dup_repo origin_uri "cloned" in
    add_remote "other" other_uri cloned_repo;
    Git.fetch cloned_repo;
    let versions = Git.versions cloned_repo in

    assert_prerelease ~ctxt v1_semver v1_sha versions;
    assert_prerelease ~ctxt v2_semver v2_sha versions;
    assert_prerelease ~ctxt v3_semver v3_sha versions
  in
  let test_tags ctxt =
    let (origin_path, origin_uri, origin_repo) = make_repo () in

    let (v1_sha, v1_semver) = tag "foo" origin_path 1 2 3 origin_repo in
    let (v2_sha, v2_semver) = tag "bar" origin_path 2 3 4 origin_repo in
    let (v3_sha, v3_semver) = tag "baz" origin_path 3 4 5 origin_repo in
    let (_, ignored_semver) = tag ~ignore:true "quux" origin_path 4 5 6 origin_repo in

    let (_, _, cloned_repo) = dup_repo origin_uri "cloned" in
    let versions = Git.versions cloned_repo in

    assert_latest ~ctxt v1_semver v1_sha versions;
    assert_latest ~ctxt v2_semver v2_sha versions;
    assert_latest ~ctxt v3_semver v3_sha versions;
    assert_no_latest ignored_semver versions
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
