open Format

open OUnit2

open Common
open Syntax

open CommonTest

(* Assertions *)

let assert_parses = ParseTest.assert_parses Lexer.lex

let assert_parses_pkg = assert_parses Parser.pkg_test AstTest.assert_pkg_equal
let assert_parses_import = assert_parses Parser.import_test AstTest.assert_import_equal
let assert_parses_top = assert_parses Parser.top_test AstTest.assert_top_equal
let assert_parses_binding = assert_parses Parser.binding_test AstTest.assert_binding_equal
let assert_parses_patt = assert_parses Parser.patt_test AstTest.assert_patt_equal
let assert_parses_expr = assert_parses Parser.expr_test AstTest.assert_expr_equal
let assert_parses_ty = assert_parses Parser.ty_test TypeTest.assert_ty_equal

let assert_parses_package_only = assert_parses Parser.package_only AstTest.assert_file_equal
let assert_parses_imports_only = assert_parses Parser.imports_only AstTest.assert_file_equal
let assert_parses_file = assert_parses Parser.file AstTest.assert_file_equal

(* Types *)

let test_parse_ty_constr ctxt =
  let id = "TypeConstructor" in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 15, 15) in
  ()
    |> Sym.seq
    |> Sym.gen ~id:id
    |> Type.constr loc
    |> assert_parses_ty ~ctxt env [id]

(* Expressions *)

let test_parse_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 4, 4) in
  Ast.bool loc true
    |> assert_parses_expr ~ctxt env ["true"]

let test_parse_expr_int ctxt =
  let lexeme = "-42i" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 4, 4) in
  Ast.int loc lexeme
    |> assert_parses_expr ~ctxt env [lexeme]

let test_parse_expr_long ctxt =
  let lexeme = "-42l" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 4, 4) in
  Ast.long loc lexeme
    |> assert_parses_expr ~ctxt env [lexeme]

let test_parse_expr_float ctxt =
  let lexeme = "-4.2f" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 5, 5) in
  Ast.float loc lexeme
    |> assert_parses_expr ~ctxt env [lexeme]

let test_parse_expr_double ctxt =
  let lexeme = "-4.2d" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 5, 5) in
  Ast.double loc lexeme
    |> assert_parses_expr ~ctxt env [lexeme]

let test_parse_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 3, 3) in
  'a'
    |> Uchar.of_char
    |> Ast.rune loc
    |> assert_parses_expr ~ctxt env ["'a'"]

let test_parse_expr_string ctxt =
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 9, 9) in
  "foo bar"
    |> String.to_seq
    |> List.of_seq
    |> List.map Uchar.of_char
    |> Ast.string loc
    |> assert_parses_expr ~ctxt env ["\"foo bar\""]

let test_parse_expr_lident ctxt =
  let id = "lowerIdent" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 10, 10) in
  ()
    |> Sym.seq
    |> Sym.gen ~id
    |> Ast.ident loc
    |> assert_parses_expr ~ctxt env [id]

let test_parse_expr_uident ctxt =
  let id = "UpperIdent" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 10, 10) in
  ()
    |> Sym.seq
    |> Sym.gen ~id
    |> Ast.ident loc
    |> assert_parses_expr ~ctxt env [id]

(* Patterns *)

let test_parse_patt_ground ctxt =
  let env = EnvTest.fresh () in
  LocTest.make (1, 0, 0) (1, 1, 1)
    |> Ast.patt_ground
    |> assert_parses_patt ~ctxt env ["_"]

let test_parse_patt_var ctxt =
  let id = "ident" in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 5, 5) in
  ()
    |> Sym.seq
    |> Sym.gen ~id
    |> Ast.patt_var loc
    |> assert_parses_patt ~ctxt env [id]

(* Bindings *)

let test_parse_value_binding_implicit ctxt =
  let env = EnvTest.fresh () in
  let value =
    let loc = LocTest.make (1, 4, 4) (1, 8, 8) in
    Ast.bool loc true
  in
  let patt =
    LocTest.make (1, 0, 0) (1, 1, 1)
      |> Ast.patt_ground
  in
  let loc = LocTest.make (1, 0, 0) (1, 8, 8) in
  Ast.value_binding loc patt None value
    |> assert_parses_binding ~ctxt env ["_ = true"]

let test_parse_value_binding_explicit ctxt =
  let seq = Sym.seq () in
  let id = "testPatt" in
  let value =
    let loc = LocTest.make (1, 17, 17) (1, 21, 21) in
    Ast.bool loc true
  in
  let patt =
    let loc = LocTest.make (1, 0, 0) (1, 8, 8) in
    seq
      |> Sym.gen ~id
      |> Ast.patt_var loc
  in
  let ty =
    let loc = LocTest.make (1, 10, 10) (1, 14, 14) in
    seq
      |> Sym.gen ~id:Prim.id_bool
      |> Type.constr loc
  in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 21, 21) in
  Ast.value_binding loc patt (Some ty) value
    |> assert_parses_binding ~ctxt env [
         sprintf "%s: %s = true" id Prim.id_bool
       ]

(* Top-Level Expressions *)

let test_parse_top_let ctxt =
  let binding =
    let value =
      let loc = LocTest.make (1, 8, 8) (1, 12, 12) in
      Ast.bool loc true
    in
    let patt =
      LocTest.make (1, 4, 4) (1, 5, 5)
        |> Ast.patt_ground
    in
    let loc = LocTest.make (1, 4, 4) (1, 12, 12) in
    Ast.value_binding loc patt None value
  in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 12, 12) in
  Ast.top_let loc binding
    |> assert_parses_top ~ctxt env ["let _ = true"]

let test_parse_top_val ctxt =
  let binding =
    let value =
      let loc = LocTest.make (1, 8, 8) (1, 12, 12) in
      Ast.bool loc true
    in
    let patt =
      LocTest.make (1, 4, 4) (1, 5, 5)
        |> Ast.patt_ground
    in
    let loc = LocTest.make (1, 4, 4) (1, 12, 12) in
    Ast.value_binding loc patt None value
  in
  let env = EnvTest.fresh () in
  let loc = LocTest.make (1, 0, 0) (1, 12, 12) in
  Ast.top_val loc binding
    |> assert_parses_top ~ctxt env ["val _ = true"]

(* Imports *)

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
  Ast.file pkg [] []
    |> assert_parses_package_only ~ctxt env [
         sprintf "package %s" id
       ];

  let env = EnvTest.fresh () in
  Ast.file pkg [] []
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
  Ast.file pkg [] []
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
  Ast.file pkg [] []
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
    "Types" >::: [
      "Constructors" >:: test_parse_ty_constr;
    ];
    "Expressions" >::: [
      "Booleans"               >:: test_parse_expr_bool;
      "Integers"               >:: test_parse_expr_int;
      "Longs"                  >:: test_parse_expr_long;
      "Floats"                 >:: test_parse_expr_float;
      "Doubles"                >:: test_parse_expr_double;
      "Runes"                  >:: test_parse_expr_rune;
      "Strings"                >:: test_parse_expr_string;
      "Lower-case Identifiers" >:: test_parse_expr_lident;
      "Upper-case Identifiers" >:: test_parse_expr_uident;
    ];
    "Patterns" >::: [
      "Ground"     >:: test_parse_patt_ground;
      "Identifier" >:: test_parse_patt_var;
    ];
    "Bindings" >::: [
      "Value Bindings" >::: [
        "Implicit" >:: test_parse_value_binding_implicit;
        "Explicit" >:: test_parse_value_binding_explicit;
      ];
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_parse_top_let;
      "Val Bindings" >:: test_parse_top_val;
    ];
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
