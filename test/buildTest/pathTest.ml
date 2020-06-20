open Format

open OUnit2

open Build

let test_src =
  let test_internal _ =
    match Path.internal with
      | Path.Internal -> ()
      | _ -> assert_failure "expected internal source"
  in
  let test_github =
    let owner = "the-owner" in
    let repo = "the-repo" in
    let ver = 42 in

    let test_valid ctxt =
      match Path.github owner repo ver with
        | Path.GitHub(owner', repo', ver') ->
          assert_equal ~ctxt owner owner';
          assert_equal ~ctxt repo repo';
          assert_equal ~ctxt ver ver'
        | _ -> assert_failure "expected GitHub source"
    in
    let test_invalid =
      let test owner repo ver msg _ =
        let msg = sprintf "GitHub %s cannot be blank" msg in
        let exn = Invalid_argument msg in
        let fn _ = Path.github owner repo ver in
        assert_raises exn fn
      in
      let test_negative_version _ =
        let exn = Invalid_argument "major version cannot be negative" in
        let fn _ = Path.github owner repo (-1) in
        assert_raises exn fn
      in
      let test_owner_and_repo = test "" "" ver "owner and repo" in
      let test_owner = test "" repo ver "owner" in
      let test_repo = test owner "" ver "repo" in
      "Invalid" >::: [
        "Owner and Repo"          >:: test_owner_and_repo;
        "Owner"                   >:: test_owner;
        "Repo"                    >:: test_repo;
        "Negative Version Number" >:: test_negative_version
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
    let ver = 42 in

    let test_valid ctxt =
      match Path.ext (Some proto) host (Some port) (Some path) ver with
        | Path.External(Some proto', host', Some port', Some path', ver') ->
          assert_equal ~ctxt proto proto';
          assert_equal ~ctxt host host';
          assert_equal ~ctxt port port';
          assert_equal ~ctxt path path';
          assert_equal ~ctxt ver ver'
        | _ ->
          assert_failure "expected external source"
    in
    let test_invalid =
      let test_negative_version _ =
        let exn = Invalid_argument "major version cannot be negative" in
        let fn _ = Path.ext None host None None (-1) in
        assert_raises exn fn
      in
      let test_blank_host _ =
        let exn = Invalid_argument "host cannot be blank" in
        let fn _ = Path.ext None "" None None ver in
        assert_raises exn fn
      in
      let test_invalid_host _ =
        let invalid = "foo" in
        let msg = sprintf "%S is not a valid host" invalid in
        let exn = Invalid_argument msg in
        let fn _ = Path.ext None invalid None None ver in
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
    "Internal" >:: test_internal;
    test_github;
    test_external
  ]

let test_create =
  let src = Path.internal in
  let pkg = "the/pkg" in
  let recur = true in

  let test_valid ctxt =
    let path = Path.create src pkg recur in

    assert_equal ~ctxt src path.source;
    assert_equal ~ctxt pkg path.package;
    assert_equal ~ctxt recur path.recursive
  in
  "Create" >::: [
    "Valid" >:: test_valid
  ]

let test_of_string =
  let test_source =
    let ver = 42 in

    let test_internal ctxt =
      let path = Path.of_string ".:" in

      assert_bool "expected non-recursive path" (not path.recursive);
      assert_equal ~ctxt "" path.package;
      match path.source with
        | Path.Internal -> ()
        | _ -> assert_failure "expected internal path"
    in
    let test_github =
      let owner = "the-owner" in
      let repo = "the-repo" in

      let test_valid ctxt =
        let p = sprintf "github.com/%s/%s@v%d:" owner repo ver in
        let path = Path.of_string p in

        assert_bool "expected non-recursive path" (not path.recursive);
        assert_equal ~ctxt "" path.package;
        match path.source with
          | Path.GitHub(owner', repo', ver') ->
            assert_equal ~ctxt owner owner';
            assert_equal ~ctxt repo repo';
            assert_equal ~ctxt ver ver'
          | _ -> assert_failure "expected GitHub path"
      in
      let test_invalid =
        let test_extra_path_segments _ =
          let extra = "/foo/bar" in
          let p = sprintf "github.com/%s/%s%s@v%d:" owner repo extra ver in

          let msg = sprintf "GitHub source contains extra path segments %S" extra in
          let exn = Invalid_argument msg in
          let fn _ = Path.of_string p in

          assert_raises exn fn
        in
        "Invalid" >::: [
          "Extra Path Segments" >:: test_extra_path_segments
        ]
      in
      "GitHub" >::: [
        "Valid"   >:: test_valid;
        test_invalid
      ]
    in
    let test_external =
      let proto = "the-proto" in
      let host = "sub-1.sub-2.the-host.co.uk" in
      let port = 1234 in
      let path = "the/path" in

      let test_host_only ctxt =
        let p = sprintf "%s@v%d:" host ver in
        let path = Path.of_string p in

        assert_bool "expected non-recursive path" (not path.recursive);
        assert_equal ~ctxt "" path.package;
        match path.source with
          | Path.External(None, host', None, None, ver') ->
            assert_equal ~ctxt host host';
            assert_equal ~ctxt ver ver'
          | _ -> assert_failure "expected external path"
      in
      let test_protocol ctxt =
        let p = sprintf "%s://%s@v%d:" proto host ver in
        let path = Path.of_string p in

        assert_bool "expected non-recursive path" (not path.recursive);
        assert_equal ~ctxt "" path.package;
        match path.source with
          | Path.External(Some proto', host', None, None, ver') ->
            assert_equal ~ctxt proto proto';
            assert_equal ~ctxt host host';
            assert_equal ~ctxt ver ver'
          | _ -> assert_failure "expected external path"
      in
      let test_port ctxt =
        let p = sprintf "%s:%d@v%d:" host port ver in
        let path = Path.of_string p in

        assert_bool "expected non-recursive path" (not path.recursive);
        assert_equal ~ctxt "" path.package;
        match path.source with
          | Path.External(None, host', Some port', None, ver') ->
            assert_equal ~ctxt host host';
            assert_equal ~ctxt port port';
            assert_equal ~ctxt ver ver'
          | _ -> assert_failure "expected external path"
      in
      let test_path ctxt =
        let p = sprintf "%s/%s@v%d:" host path ver in
        let path' = Path.of_string p in

        assert_bool "expected non-recursive path" (not path'.recursive);
        assert_equal ~ctxt "" path'.package;
        match path'.source with
          | Path.External(None, host', None, Some path', ver') ->
            assert_equal ~ctxt host host';
            assert_equal ~ctxt path path';
            assert_equal ~ctxt ver ver'
          | _ -> assert_failure "expected external path"
      in
      let test_all ctxt =
        let p = sprintf "%s://%s:%d/%s@v%d:" proto host port path ver in
        let path' = Path.of_string p in

        assert_bool "expected non-recursive path" (not path'.recursive);
        assert_equal ~ctxt "" path'.package;
        match path'.source with
          | Path.External(Some proto', host', Some port', Some path', ver') ->
            assert_equal ~ctxt proto proto';
            assert_equal ~ctxt host host';
            assert_equal ~ctxt port port';
            assert_equal ~ctxt path path';
            assert_equal ~ctxt ver ver'
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
    "Source" >::: [
      "Internal" >:: test_internal;
      test_github;
      test_external
    ]
  in
  let test_path =
    let pkg = "the/pkg" in

    let test_blank ctxt =
      let p = ".:" in
      let path = Path.of_string p in

      assert_equal ~ctxt Path.internal path.source;
      assert_bool "expected non-recursive path" (not path.recursive);
      assert_equal ~ctxt "" path.package
    in
    let test_all ctxt =
      let p = ".:..." in
      let path = Path.of_string p in

      assert_equal ~ctxt Path.internal path.source;
      assert_bool "expected recursive path" path.recursive;
      assert_equal ~ctxt "" path.package
    in
    let test_path ctxt =
      let p = sprintf ".:%s" pkg in
      let path = Path.of_string p in

      assert_equal ~ctxt Path.internal path.source;
      assert_bool "expected non-recursive path" (not path.recursive);
      assert_equal ~ctxt pkg path.package
    in
    let test_rec ctxt =
      let p = sprintf ".:%s/..." pkg in
      let path = Path.of_string p in

      assert_equal ~ctxt Path.internal path.source;
      assert_bool "expected recursive path" path.recursive;
      assert_equal ~ctxt ~printer:(fun x -> x) pkg path.package
    in
    "Path" >::: [
      "Blank"     >:: test_blank;
      "All"       >:: test_all;
      "Path"      >:: test_path;
      "Recursive" >:: test_rec
    ]
  in
  "Parsing" >::: [
    test_source;
    test_path
  ]

let test_format =
  let assert_formats ~ctxt expected semver =
    Path.format str_formatter semver;
    let actual = flush_str_formatter () in
    let printer x = x in
    assert_equal ~ctxt ~printer expected actual
  in

  let src = Path.internal in
  let ver = 42 in
  let pkg = "" in
  let recur = false in

  let test_source =
    let test_internal ctxt =
      let src = Path.internal in
      let path = Path.create src pkg recur in

      assert_formats ~ctxt ".:" path
    in
    let test_github ctxt =
      let owner = "the-owner" in
      let repo = "the-repo" in
      let src = Path.github owner repo ver in
      let path = Path.create src pkg recur in

      let expected = sprintf "github.com/%s/%s@v%d:" owner repo ver in
      assert_formats ~ctxt expected path
    in
    let test_external =
      let proto = "the-proto" in
      let host = "sub-1.sub-2.the-host.co.uk" in
      let port = 1234 in
      let path = "the/path" in

      let test_host_only ctxt =
        let src = Path.ext None host None None ver in
        let path = Path.create src pkg recur in

        let expected = sprintf "%s@v%d:" host ver in
        assert_formats ~ctxt expected path
      in
      let test_proto ctxt =
        let src = Path.ext (Some proto) host None None ver in
        let path = Path.create src pkg recur in

        let expected = sprintf "%s://%s@v%d:" proto host ver in
        assert_formats ~ctxt expected path
      in
      let test_port ctxt =
        let src = Path.ext None host (Some port) None ver in
        let path = Path.create src pkg recur in

        let expected = sprintf "%s:%d@v%d:" host port ver in
        assert_formats ~ctxt expected path
      in
      let test_path ctxt =
        let src = Path.ext None host None (Some path) ver in
        let path' = Path.create src pkg recur in

        let expected = sprintf "%s/%s@v%d:" host path ver in
        assert_formats ~ctxt expected path'
      in
      let test_all ctxt =
        let src = Path.ext (Some proto) host (Some port) (Some path) ver in
        let path' = Path.create src pkg recur in

        let expected = sprintf "%s://%s:%d/%s@v%d:" proto host port path ver in
        assert_formats ~ctxt expected path'
      in
      "External" >::: [
        "Host only" >:: test_host_only;
        "Protocol"  >:: test_proto;
        "Port"      >:: test_port;
        "Path"      >:: test_path;
        "All"       >:: test_all
      ]
    in
    "Source" >::: [
      "Internal" >:: test_internal;
      "GitHub"   >:: test_github;
      test_external
    ]
  in
  let test_path =
    let path = "" in

    let test_blank ctxt =
      let path' = Path.create src path false in
      assert_formats ~ctxt ".:" path'
    in
    let test_all ctxt =
      let path' = Path.create src path true in
      assert_formats ~ctxt ".:..." path'
    in
    let test_path ctxt =
      let path = "the/path" in
      let path' = Path.create src path false in

      let expected = sprintf ".:%s" path in
      assert_formats ~ctxt expected path'
    in
    let test_rec ctxt =
      let path = "the/path" in
      let path' = Path.create src path true in

      let expected = sprintf ".:%s/..." path in
      assert_formats ~ctxt expected path'
    in
    "Path" >::: [
      "Blank"     >:: test_blank;
      "All"       >:: test_all;
      "Path"      >:: test_path;
      "Recursive" >:: test_rec
    ]
  in
  "Formatting" >::: [
    test_source;
    test_path
  ]

let test_to_string =
  let assert_prints ~ctxt expected path =
    let actual = Path.to_string path in
    assert_equal ~ctxt expected actual
  in

  let src = Path.internal in
  let ver = 42 in
  let pkg = "" in
  let recur = false in

  let test_source =
    let test_internal ctxt =
      let src = Path.internal in
      let path = Path.create src pkg recur in

      assert_prints ~ctxt ".:" path
    in
    let test_github ctxt =
      let owner = "the-owner" in
      let repo = "the-repo" in
      let src = Path.github owner repo ver in
      let path = Path.create src pkg recur in

      let expected = sprintf "github.com/%s/%s@v%d:" owner repo ver in
      assert_prints ~ctxt expected path
    in
    let test_external =
      let proto = "the-proto" in
      let host = "sub-1.sub-2.the-host.co.uk" in
      let port = 1234 in
      let path = "the/path" in

      let test_host_only ctxt =
        let src = Path.ext None host None None ver in
        let path = Path.create src pkg recur in

        let expected = sprintf "%s@v%d:" host ver in
        assert_prints ~ctxt expected path
      in
      let test_proto ctxt =
        let src = Path.ext (Some proto) host None None ver in
        let path = Path.create src pkg recur in

        let expected = sprintf "%s://%s@v%d:" proto host ver in
        assert_prints ~ctxt expected path
      in
      let test_port ctxt =
        let src = Path.ext None host (Some port) None ver in
        let path = Path.create src pkg recur in

        let expected = sprintf "%s:%d@v%d:" host port ver in
        assert_prints ~ctxt expected path
      in
      let test_path ctxt =
        let src = Path.ext None host None (Some path) ver in
        let path' = Path.create src pkg recur in

        let expected = sprintf "%s/%s@v%d:" host path ver in
        assert_prints ~ctxt expected path'
      in
      let test_all ctxt =
        let src = Path.ext (Some proto) host (Some port) (Some path) ver in
        let path' = Path.create src pkg recur in

        let expected = sprintf "%s://%s:%d/%s@v%d:" proto host port path ver in
        assert_prints ~ctxt expected path'
      in
      "External" >::: [
        "Host only" >:: test_host_only;
        "Protocol"  >:: test_proto;
        "Port"      >:: test_port;
        "Path"      >:: test_path;
        "All"       >:: test_all
      ]
    in
    "Source" >::: [
      "Internal" >:: test_internal;
      "GitHub"   >:: test_github;
      test_external
    ]
  in
  let test_path =
    let path = "" in

    let test_blank ctxt =
      let path' = Path.create src path false in
      assert_prints ~ctxt ".:" path'
    in
    let test_all ctxt =
      let path' = Path.create src path true in
      assert_prints ~ctxt ".:..." path'
    in
    let test_path ctxt =
      let path = "the/path" in
      let path' = Path.create src path false in

      let expected = sprintf ".:%s" path in
      assert_prints ~ctxt expected path'
    in
    let test_rec ctxt =
      let path = "the/path" in
      let path' = Path.create src path true in

      let expected = sprintf ".:%s/..." path in
      assert_prints ~ctxt expected path'
    in
    "Path" >::: [
      "Blank"     >:: test_blank;
      "All"       >:: test_all;
      "Path"      >:: test_path;
      "Recursive" >:: test_rec
    ]
  in
  "Printing" >::: [
    test_source;
    test_path
  ]

(* Test Suite *)
let suite =
  "Paths" >::: [
    test_src;
    test_create;
    test_of_string;
    test_format;
    test_to_string
  ]
