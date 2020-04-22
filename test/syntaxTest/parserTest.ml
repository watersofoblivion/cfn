open Format

open OUnit2

open Syntax

(* Assertions *)

let assert_parses_gen ~ctxt lexer parser expected lines =
  let src = String.concat "\n" lines in
  let lexbuf = Lexer.from_string src in
  let actual = parser lexer lexbuf |> Ast.deloc_file in

  let pp_diff fmt (expected, actual) =
    fprintf fmt "Expected:\n%a\nActual:\n%a\n\n" Fmt.file expected Fmt.file actual
  in
  assert_equal ~ctxt ~pp_diff expected actual

let assert_parses = assert_parses_gen Lexer.lex Parser.file
let assert_package_only = assert_parses_gen Lexer.lex Parser.package_only
let assert_imports_only = assert_parses_gen Lexer.lex Parser.imports_only

(* Fixtures *)

let pkg_name = "foo"
let package_stmt = Ast.package_stmt Loc.dummy Loc.dummy pkg_name

let import_path = "import/path"
let from_clause = Ast.from_clause Loc.dummy Loc.dummy import_path

let package_path_1 = "package/one"
let package_clause_1 = Ast.package_clause Loc.dummy package_path_1 None

let package_path_2 = "package/two"
let package_clause_2 = Ast.package_clause Loc.dummy package_path_2 None

let package_path_3 = "package/three"
let package_clause_3 = Ast.package_clause Loc.dummy package_path_3 None

let package_path_4 = "package/four"
let package_clause_4 = Ast.package_clause Loc.dummy package_path_4 None

let local_alias_1 = "alias1"
let alias_1 = Ast.package_alias Loc.dummy local_alias_1
let aliased_package_clause_1 = Ast.package_clause Loc.dummy package_path_1 (Some alias_1)

let local_alias_2 = "alias2"
let alias_2 = Ast.package_alias Loc.dummy local_alias_2
let aliased_package_clause_2 = Ast.package_clause Loc.dummy package_path_2 (Some alias_2)

(* Package Statement *)
let test_package_stmt ctxt =
  let expected = Ast.file package_stmt [] in
  assert_parses ~ctxt expected [
    sprintf "package %s" pkg_name
  ]

(* Package Only Parser *)
let test_package_only =
  let expected = Ast.file package_stmt [] in

  let test_stop_at_from ctxt =
    assert_package_only ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "from \"import/path\" import \"package/path\""
    ]
  in
  let test_stop_at_import ctxt =
    assert_package_only ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "import \"package/path\""
    ]
  in
  "Package Only" >::: [
    "Stops at Import Clause" >:: test_stop_at_from;
    "Stops at From Clause"   >:: test_stop_at_import;
  ]

(* Imports *)
let test_imports =
  let test_single_unaliased ctxt =
    let import_clause = Ast.import_clause Loc.dummy [package_clause_1] in
    let import_stmt = Ast.import_stmt None import_clause in

    let expected = Ast.file package_stmt [import_stmt] in

    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "import %S" package_path_1
    ];
    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "import | %S" package_path_1
    ]
  in
  let test_single_aliased ctxt =
    let import_clause = Ast.import_clause Loc.dummy [aliased_package_clause_1] in
    let import_stmt = Ast.import_stmt None import_clause in

    let expected = Ast.file package_stmt [import_stmt] in

    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "import %S -> %s" package_path_1 local_alias_1
    ];
    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
              "import";
      sprintf "  | %S -> %s" package_path_1 local_alias_1
    ]
  in
  let test_multiple_unaliased ctxt =
    let package_clauses = [package_clause_1; package_clause_2] in
    let import_clause = Ast.import_clause Loc.dummy package_clauses in
    let import_stmt = Ast.import_stmt None import_clause in

    let expected = Ast.file package_stmt [import_stmt] in

    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "import %S" package_path_1;
      sprintf "  | %S" package_path_2
    ];
    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
              "import";
      sprintf "  | %S" package_path_1;
      sprintf "  | %S" package_path_2
    ]
  in
  let test_multiple_aliased ctxt =
    let package_clauses = [aliased_package_clause_1; aliased_package_clause_2] in
    let import_clause = Ast.import_clause Loc.dummy package_clauses in
    let import_stmt = Ast.import_stmt None import_clause in

    let expected = Ast.file package_stmt [import_stmt] in

    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "import %S -> %s" package_path_1 local_alias_1;
      sprintf "  | %S -> %s" package_path_2 local_alias_2
    ];
    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
              "import";
      sprintf "  | %S -> %s" package_path_1 local_alias_1;
      sprintf "  | %S -> %s" package_path_2 local_alias_2
    ]
  in
  let test_mixed_aliases =
    let test_alias_first ctxt =
      let package_clauses = [aliased_package_clause_1; package_clause_2] in
      let import_clause = Ast.import_clause Loc.dummy package_clauses in
      let import_stmt = Ast.import_stmt None import_clause in

      let expected = Ast.file package_stmt [import_stmt] in

      assert_parses ~ctxt expected [
        sprintf "package %s" pkg_name;
                "";
        sprintf "import %S -> %s" package_path_1 local_alias_1;
        sprintf "  | %S" package_path_2
      ];
      assert_parses ~ctxt expected [
        sprintf "package %s" pkg_name;
                "";
                "import";
        sprintf "  | %S -> %s" package_path_1 local_alias_1;
        sprintf "  | %S" package_path_2
      ]
    in
    let test_alias_last ctxt =
      let package_clauses = [package_clause_1; aliased_package_clause_2] in
      let import_clause = Ast.import_clause Loc.dummy package_clauses in
      let import_stmt = Ast.import_stmt None import_clause in

      let expected = Ast.file package_stmt [import_stmt] in

      assert_parses ~ctxt expected [
        sprintf "package %s" pkg_name;
                "";
        sprintf "import %S" package_path_1;
        sprintf "  | %S -> %s" package_path_2 local_alias_2
      ];
      assert_parses ~ctxt expected [
        sprintf "package %s" pkg_name;
                "";
                "import";
        sprintf "  | %S" package_path_1;
        sprintf "  | %S -> %s" package_path_2 local_alias_2
      ]
    in
    "Mixed Aliases" >::: [
      "Alias First" >:: test_alias_first;
      "Alias Last"  >:: test_alias_last
    ]
  in
  let test_from_clause ctxt =
    let import_clause = Ast.import_clause Loc.dummy [package_clause_1] in
    let import_stmt = Ast.import_stmt (Some from_clause) import_clause in

    let expected = Ast.file package_stmt [import_stmt] in

    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "from %S import %S" import_path package_path_1
    ];
    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
      sprintf "from %S import | %S" import_path package_path_1
    ]
  in
  let test_multiple_imports ctxt =
    let import_stmts =
      let import_stmt_1 =
        let package_clauses = [aliased_package_clause_1; package_clause_3] in
        let import_clause = Ast.import_clause Loc.dummy package_clauses in
        Ast.import_stmt None import_clause
      in
      let import_stmt_2 =
      let package_clauses = [package_clause_4; aliased_package_clause_2] in
        let import_clause = Ast.import_clause Loc.dummy package_clauses in
        Ast.import_stmt (Some from_clause) import_clause
      in
      [import_stmt_1; import_stmt_2]
    in

    let expected = Ast.file package_stmt import_stmts in

    assert_parses ~ctxt expected [
      sprintf "package %s" pkg_name;
              "";
              "import";
      sprintf "  | %S -> %s" package_path_1 local_alias_1;
      sprintf "  | %S" package_path_3;
      sprintf "from %S import" import_path;
      sprintf "  | %S" package_path_4;
      sprintf "  | %S -> %s" package_path_2 local_alias_2
    ]
  in
  "Imports" >::: [
    "Single Unaliased Package"    >:: test_single_unaliased;
    "Single Aliased Package"      >:: test_single_aliased;
    "Multiple Unaliased Packages" >:: test_multiple_unaliased;
    "Multiple Aliased Packages"   >:: test_multiple_aliased;
    test_mixed_aliases;
    "From Clause"                 >:: test_from_clause;
    "Multiple Imports"            >:: test_multiple_imports
  ]

let test_imports_only =
  let _ = assert_imports_only in
  "Imports Only" >::: [
  ]

(* Files *)
let test_file =
  "File" >::: [
  ]

let suite =
  "Parser" >::: [
    "Package Statement" >:: test_package_stmt;
    test_package_only;
    test_imports;
    test_imports_only;
    test_file
]
