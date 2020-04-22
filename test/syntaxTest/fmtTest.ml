open Format

open OUnit2

open Syntax

(* Assertions *)

let assert_pp ~ctxt pp v lines =
  pp str_formatter v;
  let actual = flush_str_formatter () in
  let expected = String.concat "\n" lines in

  let printer s = sprintf "\n\"%s\"\n" s in
  assert_equal ~ctxt ~printer expected actual

(* Package Statement *)
let test_package ctxt =
  let pkg_name = "foo" in
  let pkg = Ast.package_stmt Loc.dummy Loc.dummy pkg_name in

  assert_pp ~ctxt Fmt.package_stmt pkg [
    sprintf "package %s" pkg_name
  ]

(* Imports *)
let test_imports =
  let test_from_clause ctxt =
    let import_path = "import/path" in
    let from = Ast.from_clause Loc.dummy Loc.dummy import_path in

    assert_pp ~ctxt Fmt.from_clause from [
      sprintf "from %S " import_path
    ]
  in
  let test_package_alias ctxt =
    let alias_name = "alias" in
    let alias = Ast.package_alias Loc.dummy alias_name in

    assert_pp ~ctxt Fmt.package_alias alias [
      sprintf " -> %s" alias_name
    ]
  in
  let test_package_clause =
    let package_path = "package/path" in

    let test_with_alias ctxt =
      let alias_name = "alias" in
      let alias = Ast.package_alias Loc.dummy alias_name in

      let package_clause = Ast.package_clause Loc.dummy package_path (Some alias) in

      assert_pp ~ctxt Fmt.package_clause package_clause [
        sprintf "%S -> %s" package_path alias_name
      ]
    in
    let test_without_alias ctxt =
      let package_clause = Ast.package_clause Loc.dummy package_path None in

      assert_pp ~ctxt Fmt.package_clause package_clause [
        sprintf "%S" package_path
      ]
    in
    "Package Clause" >::: [
      "With Alias"    >:: test_with_alias;
      "Without Alias" >:: test_without_alias
    ]
  in
  let test_import_clause =
    let test_with_no_packages ctxt =
      let import_clause = Ast.import_clause Loc.dummy [] in

      assert_pp ~ctxt Fmt.import_clause import_clause [
        "import"
      ]
    in
    let test_with_one_package =
      let package_path = "package/path" in
      let alias_name = "alias" in

      let test_with_alias ctxt =
        let alias = Ast.package_alias Loc.dummy alias_name in
        let package_clause = Ast.package_clause Loc.dummy package_path (Some alias) in
        let import_clause = Ast.import_clause Loc.dummy [package_clause] in

        assert_pp ~ctxt Fmt.import_clause import_clause [
          sprintf "import %S -> %s" package_path alias_name
        ]
      in
      let test_without_alias ctxt =
        let package_clause = Ast.package_clause Loc.dummy package_path None in
        let import_clause = Ast.import_clause Loc.dummy [package_clause] in

        assert_pp ~ctxt Fmt.import_clause import_clause [
          sprintf "import %S" package_path
        ]
      in
      "With One Package" >::: [
        "With Alias"    >:: test_with_alias;
        "Without Alias" >:: test_without_alias
      ]
    in
    let test_with_multiple_packages =
      let package_path_1 = "package/one" in
      let package_path_2 = "package/two" in

      let alias_name_1 = "foo" in
      let alias_name_2 = "bar" in

      let alias_1 = Ast.package_alias Loc.dummy alias_name_1 in
      let alias_2 = Ast.package_alias Loc.dummy alias_name_2 in

      let test_with_aliases ctxt =
        let package_clauses =
          let package_one = Ast.package_clause Loc.dummy package_path_1 (Some alias_1) in
          let package_two = Ast.package_clause Loc.dummy package_path_2 (Some alias_2) in
          [package_one; package_two]
        in
        let import_clause = Ast.import_clause Loc.dummy package_clauses in

        assert_pp ~ctxt Fmt.import_clause import_clause [
                  "import";
          sprintf "  | %S -> %s" package_path_1 alias_name_1;
          sprintf "  | %S -> %s" package_path_2 alias_name_2
        ]
      in
      let test_without_aliases ctxt =
        let package_clauses =
          let package_one = Ast.package_clause Loc.dummy package_path_1 None in
          let package_two = Ast.package_clause Loc.dummy package_path_2 None in
          [package_one; package_two]
        in

        let import_clause = Ast.import_clause Loc.dummy package_clauses in

        assert_pp ~ctxt Fmt.import_clause import_clause [
                  "import";
          sprintf "  | %S" package_path_1;
          sprintf "  | %S" package_path_2
        ]
      in
      let test_with_mixed_aliases =
        let test_alias_first ctxt =
          let package_clauses =
            let package_one = Ast.package_clause Loc.dummy package_path_1 (Some alias_1) in
            let package_two = Ast.package_clause Loc.dummy package_path_2 None in
            [package_one; package_two]
          in

          let import_clause = Ast.import_clause Loc.dummy package_clauses in

          assert_pp ~ctxt Fmt.import_clause import_clause [
                    "import";
            sprintf "  | %S -> %s" package_path_1 alias_name_1;
            sprintf "  | %S" package_path_2;
          ]
        in
        let test_alias_last ctxt =
          let package_clauses =
            let package_one = Ast.package_clause Loc.dummy package_path_1 None in
            let package_two = Ast.package_clause Loc.dummy package_path_2 (Some alias_2) in
            [package_one; package_two]
          in

          let import_clause = Ast.import_clause Loc.dummy package_clauses in

          assert_pp ~ctxt Fmt.import_clause import_clause [
                    "import";
            sprintf "  | %S" package_path_1;
            sprintf "  | %S -> %s" package_path_2 alias_name_2
          ]
        in
        "With Mixed Aliases" >::: [
          "Alias First" >:: test_alias_first;
          "Alias Last"  >:: test_alias_last
        ]
      in
      "With Multiple Packages" >::: [
        "With Aliases"    >:: test_with_aliases;
        "Without Aliases" >:: test_without_aliases;
        test_with_mixed_aliases
      ]
    in
    "Import Clause" >::: [
      "With No Packages" >:: test_with_no_packages;
      test_with_one_package;
      test_with_multiple_packages
    ]
  in
  let test_import_stmt =
    let import_path = "import/path" in

    let package_path_1 = "package/one" in
    let package_path_2 = "package/two" in

    let alias_name_1 = "foo" in
    let alias_1 = Ast.package_alias Loc.dummy alias_name_1 in

    let package_clauses =
      let package_one = Ast.package_clause Loc.dummy package_path_1 (Some alias_1) in
      let package_two = Ast.package_clause Loc.dummy package_path_2 None in
      [package_one; package_two]
    in

    let import_clause = Ast.import_clause Loc.dummy package_clauses in

    let test_with_import_path ctxt =
      let from_clause = Ast.from_clause Loc.dummy Loc.dummy import_path in
      let import_stmt = Ast.import_stmt (Some from_clause) import_clause in

      assert_pp ~ctxt Fmt.import_stmt import_stmt [
        sprintf "from %S import" import_path;
        sprintf "  | %S -> %s" package_path_1 alias_name_1;
        sprintf "  | %S" package_path_2
      ]
    in
    let test_without_import_path ctxt =
      let import_stmt = Ast.import_stmt None import_clause in

      assert_pp ~ctxt Fmt.import_stmt import_stmt [
                "import";
        sprintf "  | %S -> %s" package_path_1 alias_name_1;
        sprintf "  | %S" package_path_2
      ]
    in
    "Import Statement" >::: [
      "With Import Path"    >:: test_with_import_path;
      "Without Import Path" >:: test_without_import_path
    ]
  in
  "Imports" >::: [
    "From Clause"   >:: test_from_clause;
    "Package Alias" >:: test_package_alias;
    test_package_clause;
    test_import_clause;
    test_import_stmt
  ]

(* Files *)
let test_file =
  let pkg_name = "foo" in
  let pkg = Ast.package_stmt Loc.dummy Loc.dummy pkg_name in


  let alias_name_1 = "foo" in
  let alias_1 = Ast.package_alias Loc.dummy alias_name_1 in
  let package_path_1 = "package/one" in
  let package_path_2 = "package/two" in
  let package_clauses_1 =
    let package_one = Ast.package_clause Loc.dummy package_path_1 (Some alias_1) in
    let package_two = Ast.package_clause Loc.dummy package_path_2 None in
    [package_one; package_two]
  in
  let import_clause_1 = Ast.import_clause Loc.dummy package_clauses_1 in
  let import_stmt_1 = Ast.import_stmt None import_clause_1 in

  let import_path = "import/path" in
  let from_clause = Ast.from_clause Loc.dummy Loc.dummy import_path in
  let alias_name_2 = "bar" in
  let alias_2 = Ast.package_alias Loc.dummy alias_name_2 in
  let package_path_3 = "package/three" in
  let package_path_4 = "package/four" in
  let package_clauses_2 =
    let package_one = Ast.package_clause Loc.dummy package_path_3 None in
    let package_two = Ast.package_clause Loc.dummy package_path_4 (Some alias_2) in
    [package_one; package_two]
  in
  let import_clause_2 = Ast.import_clause Loc.dummy package_clauses_2 in
  let import_stmt_2 = Ast.import_stmt (Some from_clause) import_clause_2 in

  let test_package_only ctxt =
    let file = Ast.file pkg [] in
    assert_pp ~ctxt Fmt.file file [
      sprintf "package %s" pkg_name
    ]
  in
  let test_package_and_imports ctxt =
    let file = Ast.file pkg [import_stmt_1; import_stmt_2] in
    assert_pp ~ctxt Fmt.file file [
      sprintf "package %s" pkg_name;
              "";
              "import";
      sprintf "  | %S -> %s" package_path_1 alias_name_1;
      sprintf "  | %S" package_path_2;
      sprintf "from %S import" import_path;
      sprintf "  | %S" package_path_3;
      sprintf "  | %S -> %s" package_path_4 alias_name_2;
    ]
  in
  "Files" >::: [
    "Package Only"        >:: test_package_only;
    "Package and Imports" >:: test_package_and_imports
  ]

(* Test Suite *)
let suite =
  "Pretty Printing" >::: [
    "Package Statement" >:: test_package;
    test_imports;
    test_file
  ]
