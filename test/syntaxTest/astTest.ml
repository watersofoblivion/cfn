open OUnit2

open Syntax

(* Package statement *)
let test_package_stmt =
  let kwd_loc = Loc.mock "" (1, 0, 0) (1, 7, 7) in

  let pkg_name = "foo" in
  let name_loc = Loc.mock "" (1, 8, 8) (1, 11, 11) in

  let expected_loc = Loc.mock "" (1, 0, 0) (1, 11, 11) in

  let actual = Ast.package_stmt kwd_loc name_loc pkg_name in

  let test_package_stmt ctxt =
    assert_equal ~ctxt expected_loc actual.package_stmt_loc;
    assert_equal ~ctxt name_loc actual.name_loc;
    assert_equal ~ctxt pkg_name actual.pkg_name
  in
  let test_deloc ctxt =
    let actual = Ast.deloc_package_stmt actual in

    assert_equal ~ctxt Loc.dummy actual.package_stmt_loc;
    assert_equal ~ctxt Loc.dummy actual.name_loc;
    assert_equal ~ctxt pkg_name actual.pkg_name
  in
  "Package Statement" >::: [
    "Constructor" >:: test_package_stmt;
    "Deloc"       >:: test_deloc
  ]

(* Imports *)
let test_imports =
  let test_from_clause =
    let kwd_loc = Loc.mock "" (1, 0, 0) (1, 4, 4) in

    let import_path = "foo" in
    let ip_loc = Loc.mock "" (1, 5, 5) (1, 8, 8) in

    let expected_loc = Loc.mock "" (1, 0, 0) (1, 8, 8) in

    let actual = Ast.from_clause kwd_loc ip_loc import_path in

    let test_from_clause ctxt =
      assert_equal ~ctxt expected_loc actual.from_clause_loc;
      assert_equal ~ctxt ip_loc actual.ip_loc;
      assert_equal ~ctxt import_path actual.import_path
    in
    let test_deloc ctxt =
      let actual = Ast.deloc_from_clause actual in

      assert_equal ~ctxt Loc.dummy actual.from_clause_loc;
      assert_equal ~ctxt Loc.dummy actual.ip_loc;
      assert_equal ~ctxt import_path actual.import_path
    in
    "From Clause" >::: [
      "With Location Information" >:: test_from_clause;
      "Deloc"                     >:: test_deloc
    ]
  in
  let test_package_alias =
    let loc = Loc.mock "" (1, 0, 0) (1, 3, 3) in
    let alias = "foo" in

    let actual = Ast.package_alias loc alias in

    let test_package_alias ctxt =
      assert_equal ~ctxt loc actual.package_alias_loc;
      assert_equal ~ctxt alias actual.local_alias
    in
    let test_deloc ctxt =
      let actual = Ast.deloc_package_alias actual in

      assert_equal ~ctxt Loc.dummy actual.package_alias_loc;
      assert_equal ~ctxt alias actual.local_alias
    in
    "Package Alias" >::: [
      "With Location Information" >:: test_package_alias;
      "Deloc"                     >:: test_deloc
    ]
  in
  let test_package_clause =
    let package_path = "foo" in
    let pp_loc = Loc.mock "" (1, 0, 0) (1, 3, 3) in

    let test_with_alias =
      let alias =
        let name = "bar" in
        let loc = Loc.mock "" (1, 8, 8) (1, 11, 11) in
        Ast.package_alias loc name
      in

      let expected_loc = Loc.span pp_loc alias.package_alias_loc in
      let actual = Ast.package_clause pp_loc package_path (Some alias) in

      let test_with_alias ctxt =
        assert_equal ~ctxt expected_loc actual.package_clause_loc;
        assert_equal ~ctxt pp_loc actual.pp_loc;
        assert_equal ~ctxt package_path actual.package_path;
        assert_equal ~ctxt (Some alias) actual.alias
      in
      let test_deloc ctxt =
        let alias = Ast.deloc_package_alias alias in
        let actual = Ast.deloc_package_clause actual in

        assert_equal ~ctxt Loc.dummy actual.package_clause_loc;
        assert_equal ~ctxt Loc.dummy actual.pp_loc;
        assert_equal ~ctxt package_path actual.package_path;
        assert_equal ~ctxt (Some alias) actual.alias
      in
      "With Alias" >::: [
        "With Location Information" >:: test_with_alias;
        "Deloc"                     >:: test_deloc
      ]
    in
    let test_without_alias =
      let actual = Ast.package_clause pp_loc package_path None in

      let test_without_alias ctxt =
        assert_equal ~ctxt pp_loc actual.package_clause_loc;
        assert_equal ~ctxt pp_loc actual.pp_loc;
        assert_equal ~ctxt package_path actual.package_path;
        assert_equal ~ctxt None actual.alias
      in
      let test_deloc ctxt =
        let actual = Ast.deloc_package_clause actual in

        assert_equal ~ctxt Loc.dummy actual.package_clause_loc;
        assert_equal ~ctxt Loc.dummy actual.pp_loc;
        assert_equal ~ctxt package_path actual.package_path;
        assert_equal ~ctxt None actual.alias
      in
      "Without Alias" >::: [
        "With Location Information" >:: test_without_alias;
        "Deloc"                     >:: test_deloc
      ]
    in
    "Package Clause" >::: [
      test_with_alias;
      test_without_alias
    ]
  in
  let test_import_clause =
    let kwd_loc = Loc.mock "" (1, 0, 0) (1, 6, 6) in

    let package_clause_1 =
      let package_path = "foo" in
      let pp_loc = Loc.mock "" (1, 7, 7) (1, 10, 10) in
      Ast.package_clause pp_loc package_path None
    in
    let package_clause_2 =
      let package_path = "bar" in
      let pp_loc = Loc.mock "" (1, 11, 11) (1, 14, 14) in
      Ast.package_clause pp_loc package_path None
    in
    let package_clauses = [package_clause_1; package_clause_2] in

    let expected_loc = Loc.span kwd_loc package_clause_2.package_clause_loc in
    let actual = Ast.import_clause kwd_loc package_clauses in

    let test_import_clause ctxt =
      assert_equal ~ctxt expected_loc actual.import_clause_loc;
      assert_equal ~ctxt package_clauses actual.packages
    in
    let test_deloc ctxt =
      let package_clauses = List.map Ast.deloc_package_clause package_clauses in
      let actual = Ast.deloc_import_clause actual in

      assert_equal ~ctxt Loc.dummy actual.import_clause_loc;
      assert_equal ~ctxt package_clauses actual.packages
    in
    "Import Clause" >::: [
      "With Location Information" >:: test_import_clause;
      "Deloc"                     >:: test_deloc
    ]
  in
  let test_import_stmt =
    let import_clause =
      let kwd_loc = Loc.mock "" (1, 0, 0) (1, 6, 6) in
      Ast.import_clause kwd_loc []
    in

    let test_with_from =
      let from_clause =
        let kwd_loc = Loc.mock "" (1, 7, 7) (1, 11, 11) in
        let import_path = "foo" in
        let ip_loc = Loc.mock "" (1, 12, 12) (1, 15, 15) in
        Ast.from_clause kwd_loc ip_loc import_path
      in

      let expected_loc = Loc.span from_clause.from_clause_loc import_clause.import_clause_loc in
      let actual = Ast.import_stmt (Some from_clause) import_clause in

      let test_import_stmt ctxt =
        assert_equal ~ctxt expected_loc actual.import_stmt_loc;
        assert_equal ~ctxt (Some from_clause) actual.from;
        assert_equal ~ctxt import_clause actual.import
      in
      let test_deloc ctxt =
        let from_clause = Ast.deloc_from_clause from_clause in
        let import_clause = Ast.deloc_import_clause import_clause in
        let actual = Ast.deloc_import_stmt actual in

        assert_equal ~ctxt Loc.dummy actual.import_stmt_loc;
        assert_equal ~ctxt (Some from_clause) actual.from;
        assert_equal ~ctxt import_clause actual.import
      in
      "With From Clause" >::: [
        "With Location Information" >:: test_import_stmt;
        "Deloc"                     >:: test_deloc
      ]
    in
    let test_without_from =
      let actual = Ast.import_stmt None import_clause in

      let test_import_stmt ctxt =
        assert_equal ~ctxt import_clause.import_clause_loc actual.import_stmt_loc;
        assert_equal ~ctxt None actual.from;
        assert_equal ~ctxt import_clause actual.import
      in
      let test_deloc ctxt =
        let import_clause = Ast.deloc_import_clause import_clause in
        let actual = Ast.deloc_import_stmt actual in

        assert_equal ~ctxt Loc.dummy actual.import_stmt_loc;
        assert_equal ~ctxt None actual.from;
        assert_equal ~ctxt import_clause actual.import
      in
      "Without From Clause" >::: [
        "With Location Information" >:: test_import_stmt;
        "Deloc"                     >:: test_deloc
      ]
    in
    "Import Statement" >::: [
      test_with_from;
      test_without_from
    ]
  in
  "Imports" >::: [
    test_from_clause;
    test_package_alias;
    test_package_clause;
    test_import_clause;
    test_import_stmt;
  ]

(* Files *)
let test_file =
  let test_file ctxt =
    let pkg = Ast.package_stmt Loc.dummy Loc.dummy "the-package" in

    let imports =
      let impt_1 =
        let package_alias = Ast.package_alias Loc.dummy "alias-1" in
        let import_clause = Ast.import_clause Loc.dummy [
          Ast.package_clause Loc.dummy "package-1" None;
          Ast.package_clause Loc.dummy "package-2" (Some package_alias);
        ] in
        Ast.import_stmt None import_clause
      in
      let impt_2 =
        let from_clause = Ast.from_clause Loc.dummy Loc.dummy "a-project" in
        let package_alias = Ast.package_alias Loc.dummy "alias-2" in
        let import_clause = Ast.import_clause Loc.dummy [
          Ast.package_clause Loc.dummy "package-3" None;
          Ast.package_clause Loc.dummy "package-4" (Some package_alias);
        ] in
        Ast.import_stmt (Some from_clause) import_clause
      in
      [impt_1; impt_2]
    in

    let file = Ast.file pkg imports in

    assert_equal ~ctxt pkg file.package_stmt;
    assert_equal ~ctxt imports file.import_stmts
  in
  "File" >:: test_file

(* Test Suite *)
let suite =
  "Abstract Syntax" >::: [
    test_package_stmt;
    test_imports;
    test_file
  ]
