open Format

open OUnit2

open Common
open Syntax

open CommonTest

(* Assertions *)

let assert_parses = ParseTest.assert_parses Lexer.lex

let assert_parses_pkg = assert_parses Parser.pkg_test AstTest.assert_pkg_equal
let assert_parses_import = assert_parses Parser.import_test AstTest.assert_import_equal

let assert_parses_package_only = assert_parses Parser.package_only AstTest.assert_file_equal
let assert_parses_imports_only = assert_parses Parser.imports_only AstTest.assert_file_equal
let assert_parses_file = assert_parses Parser.file AstTest.assert_file_equal

(*  *)

let test_parse_pkg ctxt =
  let id = "testpackage" in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 19, 19) in
  let id_loc = LocTest.make (1, 8, 8) (1, 19, 19) in
  ()
    |> Sym.seq
    |> Sym.gen ~id:id
    |> Ast.name id_loc
    |> Ast.pkg loc
    |> assert_parses_pkg ~ctxt env [
         sprintf "package %s" id
       ]

let test_parse_import_from ctxt =
  let src = "testsrc" in
  let pkg = "testpkg" in
  let seq = Sym.seq () in
  let env = EnvTest.fresh () in

  let pkgs =
    let alias =
      let pkg =
        let loc = LocTest.make (1, 20, 20) (1, 27, 27) in
        seq
          |> Sym.gen ~id:pkg
          |> Ast.name loc
      in
      let loc = LocTest.make (1, 20, 20) (1, 27, 27) in
      Ast.alias loc pkg None
    in
    let loc = LocTest.make (1, 13, 13) (1, 27, 27) in
    Ast.pkgs loc [alias]
  in
  let from =
    let src =
      let loc = LocTest.make (1, 5, 5) (1, 12, 12) in
      seq
        |> Sym.gen ~id:src
        |> Ast.name loc
        |> Ast.src loc
    in
    let loc = LocTest.make (1, 0, 0) (1, 12, 12) in
    Ast.from loc src
  in
  let loc = LocTest.make (1, 0, 0) (1, 27, 27) in
  Ast.import loc (Some from) pkgs
    |> assert_parses_import ~ctxt env [
         sprintf "from %s import %s" src pkg
       ]

let test_parse_import_pipe ctxt =
  let pkg = "testpkg" in
  let seq = Sym.seq () in
  let env = EnvTest.fresh () in

  let pkgs =
    let alias =
      let pkg =
        let loc = LocTest.make (1, 9, 9) (1, 16, 16) in
        seq
          |> Sym.gen ~id:pkg
          |> Ast.name loc
      in
      let loc = LocTest.make (1, 9, 9) (1, 16, 16) in
      Ast.alias loc pkg None
    in
    let loc = LocTest.make (1, 0, 0) (1, 16, 16) in
    Ast.pkgs loc [alias]
  in
  let loc = LocTest.make (1, 0, 0) (1, 16, 16) in
  Ast.import loc None pkgs
    |> assert_parses_import ~ctxt env [
         sprintf "import | %s" pkg
       ]

let test_parse_import_no_pipe ctxt =
  let pkg = "testpkg" in
  let seq = Sym.seq () in
  let env = EnvTest.fresh () in

  let pkgs =
    let alias =
      let pkg =
        let loc = LocTest.make (1, 7, 7) (1, 14, 14) in
        seq
          |> Sym.gen ~id:pkg
          |> Ast.name loc
      in
      let loc = LocTest.make (1, 7, 7) (1, 14, 14) in
      Ast.alias loc pkg None
    in
    let loc = LocTest.make (1, 0, 0) (1, 14, 14) in
    Ast.pkgs loc [alias]
  in
  let loc = LocTest.make (1, 0, 0) (1, 14, 14) in
  Ast.import loc None pkgs
    |> assert_parses_import ~ctxt env [
         sprintf "import %s" pkg
       ]

let test_parse_import_alias ctxt =
  let pkg = "testpkg" in
  let local = "testalias" in
  let seq = Sym.seq () in

  let pkgs =
    let alias =
      let pkg =
        let loc = LocTest.make (1, 7, 7) (1, 14, 14) in
        seq
          |> Sym.gen ~id:pkg
          |> Ast.name loc
      in
      let local =
        let loc = LocTest.make (1, 18, 18) (1, 27, 27) in
        seq
          |> Sym.gen ~id:local
          |> Ast.name loc
      in
      let loc = LocTest.make (1, 7, 7) (1, 27, 27) in
      Ast.alias loc pkg (Some local)
    in
    let loc = LocTest.make (1, 0, 0) (1, 27, 27) in
    Ast.pkgs loc [alias]
  in
  let loc = LocTest.make (1, 0, 0) (1, 27, 27) in

  let env = EnvTest.fresh () in
  Ast.import loc None pkgs
    |> assert_parses_import ~ctxt env [
         sprintf "import %s -> %s" pkg local
       ]

let test_parse_file_package_only ctxt =
  let id = "testpackage" in

  let loc = LocTest.make (1, 0, 0) (1, 19, 19) in
  let id_loc = LocTest.make (1, 8, 8) (1, 19, 19) in
  let pkg =
    ()
      |> Sym.seq
      |> Sym.gen ~id:id
      |> Ast.name id_loc
      |> Ast.pkg loc
  in

  let env = EnvTest.fresh () in
  Ast.file pkg []
    |> assert_parses_package_only ~ctxt env [
         sprintf "package %s" id
       ];

  let env = EnvTest.fresh () in
  Ast.file pkg []
    |> assert_parses_package_only ~ctxt env [
         sprintf "package %s" id;
         "";
         "from ignored import";
         "  | ignored_path -> ignoredalias";
         "  | another_ignored_path"
       ]

let test_parse_file_imports_only ctxt =
  let id = "testpackage" in

  let loc = LocTest.make (1, 0, 0) (1, 19, 19) in
  let id_loc = LocTest.make (1, 8, 8) (1, 19, 19) in
  let pkg =
    ()
      |> Sym.seq
      |> Sym.gen ~id:id
      |> Ast.name id_loc
      |> Ast.pkg loc
  in

  let env = EnvTest.fresh () in
  Ast.file pkg []
    |> assert_parses_imports_only ~ctxt env [
         sprintf "package %s" id
       ](* ;

  let env = EnvTest.fresh () in
  Ast.file pkg []
    |> assert_parses_imports_only ~ctxt env [
         sprintf "package %s" id;
         "";
         "from ignored import";
         "  | ignored_path -> ignoredalias";
         "  | another_ignored_path"
       ] *)

let test_parse_file_file ctxt =
  let id = "testpackage" in

  let loc = LocTest.make (1, 0, 0) (1, 19, 19) in
  let id_loc = LocTest.make (1, 8, 8) (1, 19, 19) in
  let pkg =
    ()
      |> Sym.seq
      |> Sym.gen ~id:id
      |> Ast.name id_loc
      |> Ast.pkg loc
  in

  let env = EnvTest.fresh () in
  Ast.file pkg []
    |> assert_parses_file ~ctxt env [
         sprintf "package %s" id
       ](* ;

  let env = EnvTest.fresh () in
  Ast.file pkg []
    |> assert_parses_file ~ctxt env [
         sprintf "package %s" id;
         "";
         "from ignored import";
         "  | ignored_path -> ignoredalias";
         "  | another_ignored_path"
       ]*)

let test_parses =
  "Parses" >::: [
    "Package Statement" >:: test_parse_pkg;
    "Import Statements" >::: [
      "From Clause" >:: test_parse_import_from;
      "Packages" >::: [
        "Leading Pipe"    >:: test_parse_import_pipe;
        "No Leading Pipe" >:: test_parse_import_no_pipe;
        "With Alias"      >:: test_parse_import_alias;
      ]
    ];
    "Files" >::: [
      "Package Only" >:: test_parse_file_package_only;
      "Imports Only" >:: test_parse_file_imports_only;
      "Whole File"   >:: test_parse_file_file;
    ]
  ]

let suite =
  "Parser" >::: [
    test_parses;
  ]
