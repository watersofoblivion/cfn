open Format

open OUnit2

open Syntax

open CommonTest

(* Assertions *)

let assert_pp_name = PrettyTest.assert_pp Fmt.name
let assert_pp_src = PrettyTest.assert_pp Fmt.src
let assert_pp_from = PrettyTest.assert_pp Fmt.from
let assert_pp_alias = PrettyTest.assert_pp Fmt.alias
let assert_pp_pkgs = PrettyTest.assert_pp Fmt.pkgs
let assert_pp_import = PrettyTest.assert_pp Fmt.import
let assert_pp_pkg = PrettyTest.assert_pp Fmt.pkg
let assert_pp_file = PrettyTest.assert_pp Fmt.file

(* Pretty Printing *)

let test_name ctxt =
  let id = "testpackage" in
  AstTest.fresh_name ~id ()
    |> assert_pp_name ~ctxt [id]

let test_src ctxt =
  let name = "testsource" in
  AstTest.fresh_src ~name ()
    |> assert_pp_src ~ctxt [name]

let test_from ctxt =
  let src = "testsource" in
  AstTest.fresh_from ~src ()
    |> assert_pp_from ~ctxt [
         sprintf "from %s " src
       ]

let test_alias_no_local_name ctxt =
  let pkg = "testpkg" in
  AstTest.fresh_alias ~pkg ()
    |> assert_pp_alias ~ctxt [pkg]

let test_alias_local_name ctxt =
  let pkg = "testpkg" in
  let local = "testlocal" in
  AstTest.fresh_alias ~pkg ~local ()
    |> assert_pp_alias ~ctxt [
         sprintf "%s -> %s" pkg local
       ]

let test_pkgs ctxt =
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkgs =
    Ast.pkgs LocTest.dummy [
      AstTest.fresh_alias ~pkg ~local ();
      AstTest.fresh_alias ~pkg:pkg' ()
    ]
  in
  pkgs
    |> assert_pp_pkgs ~ctxt [
         "";
         sprintf "| %s -> %s" pkg local;
         sprintf "| %s" pkg'
       ]

let test_import_from ctxt =
  let src = "testsrc" in
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let from = AstTest.fresh_from ~src () in
  let pkgs =
    Ast.pkgs LocTest.dummy [
      AstTest.fresh_alias ~pkg ~local ();
      AstTest.fresh_alias ~pkg:pkg' ()
    ]
  in

  Ast.import LocTest.dummy (Some from) pkgs
    |> assert_pp_import ~ctxt [
         sprintf "from %s import" src;
         sprintf "                   | %s -> %s" pkg local;
         sprintf "                   | %s" pkg'
       ]

let test_import ctxt =
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkgs =
    Ast.pkgs LocTest.dummy [
      AstTest.fresh_alias ~pkg ~local ();
      AstTest.fresh_alias ~pkg:pkg' ()
    ]
  in

  Ast.import LocTest.dummy None pkgs
    |> assert_pp_import ~ctxt [
         sprintf "import";
         sprintf "      | %s -> %s" pkg local;
         sprintf "      | %s" pkg'
       ]

let test_pkg ctxt =
  let id = "testpackage" in
  ()
    |> AstTest.fresh_pkg ~id
    |> assert_pp_pkg ~ctxt [
         sprintf "package %s" id
       ]

let test_file_no_imports ctxt =
  let id = "testpackage" in

  let pkg = AstTest.fresh_pkg ~id () in
  Ast.file pkg []
    |> assert_pp_file ~ctxt [
         sprintf "package %s" id
       ]

let test_file_with_imports ctxt =
  let id = "testpackage" in
  let src = "testsrc" in
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkg_stmt = AstTest.fresh_pkg ~id () in
  let imports =
    let pkgs =
      Ast.pkgs LocTest.dummy [
        AstTest.fresh_alias ~pkg ~local ();
        AstTest.fresh_alias ~pkg:pkg' ()
      ]
    in

    let import =
      let from = AstTest.fresh_from ~src () in
      Ast.import LocTest.dummy (Some from) pkgs
    in
    let import' = Ast.import LocTest.dummy None pkgs in
    [import; import']
  in
  Ast.file pkg_stmt imports
    |> assert_pp_file ~ctxt [
         sprintf "package %s" id;
         "";
         sprintf "from %s import" src;
         sprintf "                   | %s -> %s" pkg local;
         sprintf "                   | %s" pkg';
         sprintf "import";
         sprintf "      | %s -> %s" pkg local;
         sprintf "      | %s" pkg'
       ]

(* Test Suite *)
let suite =
  "Pretty Printing" >::: [
    "Names" >:: test_name;
    "Imports" >::: [
      "Sources"     >:: test_src;
      "From Clause" >:: test_from;
      "Alias Clause" >::: [
        "With Local Name"    >:: test_alias_local_name;
        "Without Local Name" >:: test_alias_no_local_name;
      ];
      "Package List" >:: test_pkgs;
      "Import Statement" >::: [
        "From"  >:: test_import_from;
        "Local" >:: test_import;
      ];
    ];
    "Package Statements" >:: test_pkg;
    "Files" >::: [
      "With Imports" >:: test_file_with_imports;
      "No Imports"   >:: test_file_no_imports;
    ];
  ]
