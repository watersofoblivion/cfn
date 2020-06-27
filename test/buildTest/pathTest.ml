open Format

open OUnit2

open Build

let assert_invalid parse typ path =
  let msg = sprintf "%S is not a valid %s path" path typ in
  let exn = Invalid_argument msg in
  let fn _ = parse path in
  assert_raises exn fn

let assert_invalid_project = assert_invalid Path.project "project"
let assert_invalid_package = assert_invalid Path.package "package"
let assert_invalid_import = assert_invalid Path.import "import"

let test_project =
  let host = "example.com" in
  let port = 8080 in
  let path = "/foo/bar" in
  let vcs = "git" in
  let major = 42 in

  let internal =
    "."
      |> Path.project
  in
  let extern =
    sprintf "%s:%d%s.%s@v%d" host port path vcs major
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
          let str_print s = s in
          let int_print i = string_of_int i in
          let prj =
            path
              |> Path.project
          in
          prj
            |> Path.source
            |> assert_equal ~ctxt ~printer:str_print expected;
          prj
            |> Path.major
            |> assert_equal ~ctxt ~printer:int_print major
        in
        let test_none ctxt =
          let expected = sprintf "%s/.%s" host vcs in
          let path = sprintf "%s@v%d" host major in
          assert_valid ~ctxt expected path
        in
        let test_path ctxt =
          let expected = sprintf "%s%s.%s" host path vcs in
          let path = sprintf "%s%s@v%d" host path major in
          assert_valid ~ctxt expected path
        in
        let test_port ctxt =
          let expected = sprintf "%s:%d/.%s" host port vcs in
          let path = sprintf "%s:%d@v%d" host port major in
          assert_valid ~ctxt expected path
        in
        let test_vcs ctxt =
          let expected = sprintf "%s:%d/.%s" host port vcs in
          let path = sprintf "%s:%d/.%s@v%d" host port vcs major in
          assert_valid ~ctxt expected path
        in
        let test_all ctxt =
          let expected = sprintf "%s:%d%s.%s" host port path vcs in
          let path = sprintf "%s:%d%s.%s@v%d" host port path vcs major in
          assert_valid ~ctxt expected path
        in
        "Valid" >::: [
          "None"                   >:: test_none;
          "Path"                   >:: test_path;
          "Port"                   >:: test_port;
          "Version Control System" >:: test_vcs;
          "All"                    >:: test_all
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
    let test_internal _ =
      let fn _ =
        internal
          |> Path.source
      in
      assert_raises Path.InternalProject fn
    in
    let test_external ctxt =
      let expected = sprintf "%s:%d%s.%s" host port path vcs in
      extern
        |> Path.source
        |> assert_equal ~ctxt expected
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
  "Project" >::: [
    test_project;
    test_current;
    test_source;
    test_major
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
  "Package" >::: [
    test_valid;
    test_invalid
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
      let src = "example.com/foo/bar.git" in
      let major = 42 in
      let path =
        sprintf "%s@v%d:%s" src major dir
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
    test_project;
    test_package;
    test_import
  ]
