open Format

open OUnit2

open Build

let test_src =
  let test_internal_src _ =
    match Prj.internal_src with
      | Prj.Internal -> ()
      | _ -> assert_failure "expected internal source"
  in
  let test_github_src =
    let owner = "the-owner" in
    let repo = "the-repo" in
    let major = 42 in

    let test_valid ctxt =
      match Prj.github_src owner repo major with
        | Prj.GitHub(owner', repo', major') ->
          assert_equal ~ctxt owner owner';
          assert_equal ~ctxt repo repo';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected GitHub source"
    in
    let test_invalid =
      let test owner repo major msg _ =
        let msg = sprintf "GitHub %s cannot be blank" msg in
        let exn = Invalid_argument msg in
        let fn _ = Prj.github_src owner repo major in
        assert_raises exn fn
      in
      let test_negative_major_version _ =
        let exn = Invalid_argument "major version cannot be negative" in
        let fn _ = Prj.github_src owner repo (-1) in
        assert_raises exn fn
      in
      let test_owner_and_repo = test "" "" major "owner and repo" in
      let test_owner = test "" repo major "owner" in
      let test_repo = test owner "" major "repo" in
      "Invalid" >::: [
        "Owner and Repo"          >:: test_owner_and_repo;
        "Owner"                   >:: test_owner;
        "Repo"                    >:: test_repo;
        "Negative Version Number" >:: test_negative_major_version
      ]
    in
    "GitHub" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  let test_external_src =
    let proto = "the-proto" in
    let host = "sub-1.sub-2.the-host.co.uk" in
    let port = 1234 in
    let path = "the/path" in
    let major = 42 in

    let test_valid ctxt =
      match Prj.external_src (Some proto) host (Some port) (Some path) major with
        | Prj.External(Some proto', host', Some port', Some path', major') ->
          assert_equal ~ctxt proto proto';
          assert_equal ~ctxt host host';
          assert_equal ~ctxt port port';
          assert_equal ~ctxt path path';
          assert_equal ~ctxt major major'
        | _ ->
          assert_failure "expected external source"
    in
    let test_invalid =
      let test_negative_version _ =
        let exn = Invalid_argument "major version cannot be negative" in
        let fn _ = Prj.external_src None host None None (-1) in
        assert_raises exn fn
      in
      let test_blank_host _ =
        let exn = Invalid_argument "host cannot be blank" in
        let fn _ = Prj.external_src None "" None None major in
        assert_raises exn fn
      in
      let test_invalid_host _ =
        let invalid = "foo" in
        let msg = sprintf "%S is not a valid host" invalid in
        let exn = Invalid_argument msg in
        let fn _ = Prj.external_src None invalid None None major in
        assert_raises exn fn
      in
      "Invalid" >::: [
        "Blank host"              >:: test_blank_host;
        "Invalid host"            >:: test_invalid_host;
        "Negative Version Number" >:: test_negative_version
      ]
    in
    "External" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  "Source" >::: [
    "Internal" >:: test_internal_src;
    test_github_src;
    test_external_src
  ]

let test_of_string =
  let major = 42 in

  let test_internal _ =
    match Prj.of_string "." with
      | Prj.Internal -> ()
      | _ -> assert_failure "expected internal path"
  in
  let test_github =
    let owner = "the-owner" in
    let repo = "the-repo" in

    let test_valid ctxt =
      let src = sprintf "github.com/%s/%s@v%d" owner repo major in
      match Prj.of_string src with
        | Prj.GitHub(owner', repo', major') ->
          assert_equal ~ctxt owner owner';
          assert_equal ~ctxt repo repo';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected GitHub path"
    in
    let test_invalid =
      let test_extra_path_segments _ =
        let extra = "/foo/bar" in
        let src = sprintf "github.com/%s/%s%s@v%d" owner repo extra major in

        let msg = sprintf "GitHub source contains extra path segments %S" extra in
        let exn = Invalid_argument msg in
        let fn _ = Prj.of_string src in

        assert_raises exn fn
      in
      "Invalid" >::: [
        "Extra Path Segments" >:: test_extra_path_segments
      ]
    in
    "GitHub" >::: [
      "Valid" >:: test_valid;
      test_invalid
    ]
  in
  let test_external =
    let proto = "the-proto" in
    let host = "sub-1.sub-2.the-host.co.uk" in
    let port = 1234 in
    let path = "the/path" in

    let test_host_only ctxt =
      let src = sprintf "%s@v%d" host major in
      match Prj.of_string src with
        | Prj.External(None, host', None, None, major') ->
          assert_equal ~ctxt host host';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected external path"
    in
    let test_protocol ctxt =
      let src = sprintf "%s://%s@v%d" proto host major in
      match Prj.of_string src with
        | Prj.External(Some proto', host', None, None, major') ->
          assert_equal ~ctxt proto proto';
          assert_equal ~ctxt host host';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected external path"
    in
    let test_port ctxt =
      let src = sprintf "%s:%d@v%d" host port major in
      match Prj.of_string src with
        | Prj.External(None, host', Some port', None, major') ->
          assert_equal ~ctxt host host';
          assert_equal ~ctxt port port';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected external path"
    in
    let test_path ctxt =
      let src = sprintf "%s/%s@v%d:" host path major in
      match Prj.of_string src with
        | Prj.External(None, host', None, Some path', major') ->
          assert_equal ~ctxt host host';
          assert_equal ~ctxt path path';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected external path"
    in
    let test_all ctxt =
      let src = sprintf "%s://%s:%d/%s@v%d" proto host port path major in
      match Prj.of_string src with
        | Prj.External(Some proto', host', Some port', Some path', major') ->
          assert_equal ~ctxt proto proto';
          assert_equal ~ctxt host host';
          assert_equal ~ctxt port port';
          assert_equal ~ctxt path path';
          assert_equal ~ctxt major major'
        | _ -> assert_failure "expected external path"
    in
    "External" >::: [
      "Host Only" >:: test_host_only;
      "Protocol"  >:: test_protocol;
      "Port"      >:: test_port;
      "Path"      >:: test_path;
      "All"       >:: test_all
    ]
  in
  "Parsing" >::: [
    "Internal" >:: test_internal;
    test_github;
    test_external
  ]

let test_format =
  let assert_formats ~ctxt expected src =
    Prj.format str_formatter src;
    let actual = flush_str_formatter () in
    let printer x = x in
    assert_equal ~ctxt ~printer expected actual
  in

  let major = 42 in

  let test_internal ctxt =
    let src = Prj.internal_src in
    assert_formats ~ctxt "." src
  in
  let test_github ctxt =
    let owner = "the-owner" in
    let repo = "the-repo" in
    let src = Prj.github_src owner repo major in
    let expected = sprintf "github.com/%s/%s@v%d" owner repo major in
    assert_formats ~ctxt expected src
  in
  let test_external =
    let proto = "the-proto" in
    let host = "sub-1.sub-2.the-host.co.uk" in
    let port = 1234 in
    let path = "the/path" in

    let test_host_only ctxt =
      let src = Prj.external_src None host None None major in
      let expected = sprintf "%s@v%d" host major in
      assert_formats ~ctxt expected src
    in
    let test_proto ctxt =
      let src = Prj.external_src (Some proto) host None None major in
      let expected = sprintf "%s://%s@v%d" proto host major in
      assert_formats ~ctxt expected src
    in
    let test_port ctxt =
      let src = Prj.external_src None host (Some port) None major in
      let expected = sprintf "%s:%d@v%d" host port major in
      assert_formats ~ctxt expected src
    in
    let test_path ctxt =
      let src = Prj.external_src None host None (Some path) major in
      let expected = sprintf "%s/%s@v%d" host path major in
      assert_formats ~ctxt expected src
    in
    let test_all ctxt =
      let src = Prj.external_src (Some proto) host (Some port) (Some path) major in
      let expected = sprintf "%s://%s:%d/%s@v%d" proto host port path major in
      assert_formats ~ctxt expected src
    in
    "External" >::: [
      "Host only" >:: test_host_only;
      "Protocol"  >:: test_proto;
      "Port"      >:: test_port;
      "Path"      >:: test_path;
      "All"       >:: test_all
    ]
  in
  "Pretty-Printing" >::: [
    "Internal" >:: test_internal;
    "GitHub"   >:: test_github;
    test_external
  ]

let test_to_string =
  let assert_prints ~ctxt expected src =
    let actual = Prj.to_string src in
    assert_equal ~ctxt expected actual
  in

  let major = 42 in

  let test_internal ctxt =
    let src = Prj.internal_src in
    assert_prints ~ctxt "." src
  in
  let test_github ctxt =
    let owner = "the-owner" in
    let repo = "the-repo" in
    let src = Prj.github_src owner repo major in
    let expected = sprintf "github.com/%s/%s@v%d" owner repo major in
    assert_prints ~ctxt expected src
  in
  let test_external =
    let proto = "the-proto" in
    let host = "sub-1.sub-2.the-host.co.uk" in
    let port = 1234 in
    let path = "the/path" in

    let test_host_only ctxt =
      let src = Prj.external_src None host None None major in
      let expected = sprintf "%s@v%d" host major in
      assert_prints ~ctxt expected src
    in
    let test_proto ctxt =
      let src = Prj.external_src (Some proto) host None None major in
      let expected = sprintf "%s://%s@v%d" proto host major in
      assert_prints ~ctxt expected src
    in
    let test_port ctxt =
      let src = Prj.external_src None host (Some port) None major in
      let expected = sprintf "%s:%d@v%d" host port major in
      assert_prints ~ctxt expected src
    in
    let test_path ctxt =
      let src = Prj.external_src None host None (Some path) major in
      let expected = sprintf "%s/%s@v%d" host path major in
      assert_prints ~ctxt expected src
    in
    let test_all ctxt =
      let src = Prj.external_src (Some proto) host (Some port) (Some path) major in
      let expected = sprintf "%s://%s:%d/%s@v%d" proto host port path major in
      assert_prints ~ctxt expected src
    in
    "External" >::: [
      "Host only" >:: test_host_only;
      "Protocol"  >:: test_proto;
      "Port"      >:: test_port;
      "Path"      >:: test_path;
      "All"       >:: test_all
    ]
  in
  "String" >::: [
    "Internal" >:: test_internal;
    "GitHub"   >:: test_github;
    test_external
  ]

(* Test Suite *)
let suite =
  "Packages" >::: [
    test_src;
    test_of_string;
    test_format;
    test_to_string
  ]
