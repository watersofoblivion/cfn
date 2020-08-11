open Format

open OUnit2

open Artifact

let assert_invalid parse typ path =
  let msg = sprintf "%S is not a valid %s" path typ in
  let exn = Invalid_argument msg in
  let fn _ = parse path in
  assert_raises exn fn

let assert_invalid_name = assert_invalid Path.id "project id"
let assert_invalid_project = assert_invalid Path.project "project path"
let assert_invalid_package = assert_invalid Path.package "package path"
let assert_invalid_import = assert_invalid Path.import "import path"

let test_name =
  let host = "example.com" in
  let path = "/foo/bar" in
  let vcs = Path.Git in
  let ext = Path.vcs_ext vcs in

  let test_valid =
    let assert_valid ~ctxt expected path =
      let expected = Path.id expected in
      path
        |> Path.id
        |> assert_equal ~ctxt expected
    in

    let expected = sprintf "%s%s" host path in

    let test_host ctxt =
      host
        |> assert_valid ~ctxt host
    in
    let test_host_path ctxt =
      sprintf "%s%s" host path
        |> assert_valid ~ctxt expected
    in
    let test_vcs_ext ctxt =
      sprintf "%s%s%s" host path ext
        |> assert_valid ~ctxt expected
    in
    "Valid" >::: [
      "Host Only"     >:: test_host;
      "Host and Path" >:: test_host_path;
      "VCS Extension" >:: test_vcs_ext
    ]
  in
  let test_invalid =
    let test_port _ =
      sprintf "%s:1234%s" host path
        |> assert_invalid_name
    in
    let test_major _ =
      sprintf "%s%s@v42" host path
        |> assert_invalid_name
    in
    let test_illegal_char _ =
      sprintf "foo!bar%s" path
        |> assert_invalid_name
    in
    let test_trailing_slash _ =
      sprintf "%s%s/" host path
        |> assert_invalid_name
    in
    "Invalid" >::: [
      "Port"              >:: test_port;
      "Major Version"     >:: test_major;
      "Illegal Character" >:: test_illegal_char;
      "Trailing Slash"    >:: test_trailing_slash;
    ]
  in
  "Names" >::: [
    test_valid;
    test_invalid
  ]

let test_project =
  let host = "example.com" in
  let path = "/foo/bar" in
  let vcs = Path.Git in
  let ext = Path.vcs_ext vcs in
  let major = 42 in

  let test_vcs =
    let test_vcs_of_ext =
      let test_git ctxt =
        ".git"
          |> Path.vcs_of_ext
          |> assert_equal ~ctxt (Some Path.Git)
      in
      let test_unrecognized ctxt =
        ".unrecognized"
          |> Path.vcs_of_ext
          |> assert_equal ~ctxt None
      in
      let test_blank ctxt =
        ""
          |> Path.vcs_of_ext
          |> assert_equal ~ctxt None
      in
      "By Extension" >::: [
        "Git"          >:: test_git;
        "Unrecognized" >:: test_unrecognized;
        "Blank"        >:: test_blank
      ]
    in
    let test_vcs_ext =
      let test_git ctxt =
        Path.Git
          |> Path.vcs_ext
          |> assert_equal ~ctxt ".git"
      in
      "Extension" >::: [
        "Git" >:: test_git
      ]
    in
    "Version Control System" >::: [
      test_vcs_of_ext;
      test_vcs_ext;
    ]
  in

  let internal =
    "."
      |> Path.project
  in
  let extern =
    sprintf "%s%s%s@v%d" host path ext major
      |> Path.project
  in

  let test_project =
    let test_internal _ =
      "."
        |> Path.project
        |> Path.current
        |> assert_bool "expected internal"
    in
    let test_external =
      let test_valid =
        let assert_valid ~ctxt expected path =
          let prj =
            path
              |> Path.project
          in
          prj
            |> Path.source
            |> assert_equal ~ctxt expected;
          prj
            |> Path.major
            |> assert_equal ~ctxt major
        in
        let test_none ctxt =
          let expected = sprintf "%s/" host in
          sprintf "%s@v%d" host major
            |> assert_valid ~ctxt expected
        in
        let test_path ctxt =
          let expected = sprintf "%s%s" host path in
          sprintf "%s%s@v%d" host path major
            |> assert_valid ~ctxt expected
        in
        let test_vcs ctxt =
          let expected = sprintf "%s/foo" host in
          let path = sprintf "%s/foo%s@v%d" host ext major in
          path
            |> assert_valid ~ctxt expected;
          path
            |> Path.project
            |> Path.vcs
            |> assert_equal ~ctxt vcs
        in
        let test_all ctxt =
          let expected = sprintf "%s%s" host path in
          sprintf "%s%s%s@v%d" host path ext major
            |> assert_valid ~ctxt expected
        in
        "Valid" >::: [
          "None" >:: test_none;
          "Path" >:: test_path;
          "VCS"  >:: test_vcs;
          "All"  >:: test_all
        ]
      in
      let test_invalid =
        let assert_invalid path =
          let msg = sprintf "%S is not a valid project path" path in
          let exn = Invalid_argument msg in
          let fn _ =
            path
              |> Path.project
          in
          assert_raises exn fn
        in

        let test_no_major _ =
          assert_invalid "example.com/foo/bar"
        in
        let test_illegal_char _ =
          assert_invalid "illegal{char}s.com/foo/bar@v42"
        in
        let test_trailing_slash _ =
          assert_invalid "illegal{char}s.com/foo/bar/@v42"
        in
        "Invalid" >::: [
          "No Major Version"  >:: test_no_major;
          "Illegal Character" >:: test_illegal_char;
          "Trailing Slash"    >:: test_trailing_slash;
        ]
      in
      "External" >::: [
        test_valid;
        test_invalid
      ]
    in
    "Parsing" >::: [
      "Internal" >:: test_internal;
      test_external
    ]
  in
  let test_current =
    let test_internal _ =
      internal
        |> Path.current
        |> assert_bool "expected true"
    in
    let test_external _ =
      extern
        |> Path.current
        |> not
        |> assert_bool "expected false"
    in
    "Current" >::: [
      "Internal" >:: test_internal;
      "External" >:: test_external
    ]
  in
  let test_source =
    let test_internal ctxt =
      internal
        |> Path.source
        |> assert_equal ~ctxt "."
    in
    let test_external ctxt =
      let expected = sprintf "%s%s" host path in
      extern
        |> Path.source
        |> assert_equal ~ctxt expected;
      extern
        |> Path.vcs
        |> assert_equal ~ctxt vcs
    in
    "Source" >::: [
      "Internal" >:: test_internal;
      "External" >:: test_external
    ]
  in
  let test_major =
    let test_internal _ =
      let fn _ =
        internal
          |> Path.major
      in
      assert_raises Path.InternalProject fn
    in
    let test_external ctxt =
      extern
        |> Path.major
        |> assert_equal ~ctxt major
    in
    "Major Version" >::: [
      "Internal" >:: test_internal;
      "External" >:: test_external
    ]
  in
  let test_compare =
    let test_internal_internal ctxt =
      let prj = Path.project "." in
      let prj' = Path.project "." in
      Path.compare_project prj prj'
        |> assert_equal ~ctxt 0
    in
    let test_internal_external ctxt =
      let prj = Path.project "." in
      let prj' = Path.project "foo.com/bar@v42" in
      Path.compare_project prj prj'
        |> assert_equal ~ctxt (-1)
    in
    let test_external_internal ctxt =
      let prj = Path.project "foo.com/bar@v42" in
      let prj' = Path.project "." in
      Path.compare_project prj prj'
        |> assert_equal ~ctxt 1
    in
    let test_external_external =
      let test_equal ctxt =
        let prj = Path.project "foo.com/bar@v42" in
        Path.compare_project prj prj
          |> assert_equal ~ctxt 0
      in
      let test_source ctxt =
        let prj = Path.project "bar.com/foo@v42" in
        let prj' = Path.project "baz.com/foo@v42" in
        let prj'' = Path.project "bar.com/foo@v41" in

        Path.compare_project prj prj'
          |> assert_equal ~ctxt (-1);
        Path.compare_project prj' prj
          |> assert_equal ~ctxt 1;
        Path.compare_project prj' prj''
          |> assert_equal ~ctxt 1
      in
      let test_major ctxt =
        let prj = Path.project "foo.com/bar@v1" in
        let prj' = Path.project "foo.com/bar@v2" in

        Path.compare_project prj prj'
          |> assert_equal ~ctxt (-1);
        Path.compare_project prj' prj
          |> assert_equal ~ctxt 1
      in
      "Externals" >::: [
        "Equal"  >:: test_equal;
        "Source" >:: test_source;
        "Major"  >:: test_major
      ]
    in
    "Compare" >::: [
      "Internal are Equal"                >:: test_internal_internal;
      "Internal is less than External"    >:: test_internal_external;
      "External is greater than Internal" >:: test_external_internal;
      test_external_external
    ]
  in
  "Project" >::: [
    test_vcs;
    test_project;
    test_current;
    test_source;
    test_major;
    test_compare
  ]

let test_package =
  let test_valid =
    let assert_pkg ~ctxt expected actual =
      actual
        |> Path.package
        |> Path.path
        |> assert_equal ~ctxt expected
    in

    let expected = "the/package/path" in

    let test_blank ctxt =
      let pkg = "" in
      pkg
        |> assert_pkg ~ctxt pkg
    in
    let test_dir ctxt =
      expected
        |> assert_pkg ~ctxt expected
    in
    let test_segments =
      let test_current ctxt =
        "the/./package/./path"
          |> assert_pkg ~ctxt expected
      in
      let test_parent ctxt =
        "the/ignored/../package/ignored-too/../path"
          |> assert_pkg ~ctxt expected
      in
      "Segments" >::: [
        "Current Directory" >:: test_current;
        "Parent Directory"  >:: test_parent;
      ]
    in
    "Valid" >::: [
      "Blank" >:: test_blank;
      "Dir"   >:: test_dir;
      test_segments
    ]
  in
  let test_invalid =
    let test_illegal_char _ =
      "dir/with:illegal/char"
        |> assert_invalid_package
    in
    let test_not_relative _ =
      "/non/relative/path"
        |> assert_invalid_package
    in
    let test_trailing_slash _ =
      "trailing/slash/"
        |> assert_invalid_package
    in
    "Invalid" >::: [
      "Illegal Character" >:: test_illegal_char;
      "Not Relative"      >:: test_not_relative;
      "Trailing Slash"    >:: test_trailing_slash
    ]
  in
  let test_compare =
    let test_lexographically ctxt =
      let path = Path.package "bar/baz" in
      let path' = Path.package "bar/quux" in
      let path'' = Path.package "baz/bar" in

      Path.compare_package path path'
        |> assert_equal ~ctxt (-1);
      Path.compare_package path' path
        |> assert_equal ~ctxt 1;
      Path.compare_package path path''
        |> assert_equal ~ctxt (-1);
      Path.compare_package path' path''
        |> assert_equal ~ctxt (-1);
      Path.compare_package path'' path
        |> assert_equal ~ctxt 1;
      Path.compare_package path'' path
        |> assert_equal ~ctxt 1;
    in
    "Compare" >::: [
      "Lexographically" >:: test_lexographically
    ]
  in
  "Package" >::: [
    test_valid;
    test_invalid;
    test_compare
  ]

let test_import =
  let test_valid =
    let dir = "baz/quux" in
    let test_internal ctxt =
      let path =
        dir
          |> sprintf ".:%s"
          |> Path.import
      in
      path
        |> Path.prj
        |> Path.current
        |> assert_bool "expected current project";
      path
        |> Path.pkg
        |> Path.path
        |> assert_equal ~ctxt dir
    in
    let test_external ctxt =
      let src = "example.com/foo/bar" in
      let vcs = Path.Git in
      let ext = Path.vcs_ext vcs in
      let major = 42 in
      let path =
        sprintf "%s%s@v%d:%s" src ext major dir
          |> Path.import
      in
      let prj = Path.prj path in
      prj
        |> Path.current
        |> not
        |> assert_bool "expected external project";
      prj
        |> Path.source
        |> assert_equal ~ctxt src;
      prj
        |> Path.vcs
        |> assert_equal ~ctxt vcs;
      prj
        |> Path.major
        |> assert_equal ~ctxt major
    in
    let test_recursive =
      let assert_rec path =
        path
          |> Path.import
          |> Path.recursive
          |> assert_bool "expected recursive import path"
      in
      let assert_non_rec path =
        path
          |> Path.import
          |> Path.recursive
          |> not
          |> assert_bool "expected non-recursive import path"
      in

      let test_non_rec _ =
        ".:"
          |> assert_non_rec;
        ".:foo/bar"
          |> assert_non_rec
      in
      let test_rec _ =
        ".:..."
          |> assert_rec;
        ".:foo/bar/..."
          |> assert_rec
      in
      "Recursion" >::: [
        "Non-recursive" >:: test_non_rec;
        "Recursive"     >:: test_rec
      ]
    in
    "Valid" >::: [
      "Internal" >:: test_internal;
      "External" >:: test_external;
      test_recursive
    ]
  in
  let test_invalid =
    let test_invalid_project _ =
      "invalid-project:valid/package"
        |> assert_invalid_project
    in
    let test_invalid_package _ =
      "valid-project@v42:/invalid/package"
        |> assert_invalid_package
    in
    let test_illegal_char _ =
      "asdf!fdsa"
        |> assert_invalid_import
    in
    let test_no_separator _ =
      "foo/bar@v42"
        |> assert_invalid_import;
      "foo/bar"
        |> assert_invalid_import
    in
    "Invalid" >::: [
      "Invalid Project"   >:: test_invalid_project;
      "Invalid Package"   >:: test_invalid_package;
      "Illegal Character" >:: test_illegal_char;
      "No Separator"      >:: test_no_separator
    ]
  in
  "Import" >::: [
    test_valid;
    test_invalid
  ]

(* Test Suite *)
let suite =
  "Paths" >::: [
    test_name;
    test_project;
    test_package;
    test_import
  ]
