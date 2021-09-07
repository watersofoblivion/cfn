(* Source Files *)

open Format

open OUnit2

open Common

open CommonTest

(* Location Stripping *)

let deloc_file = function
  | Syntax.File file ->
    let pkg = ImportTest.deloc_pkg file.pkg in
    let imports =
      file.imports
        |> List.map ImportTest.deloc_import
    in
    file.tops
      |> List.map AstTest.deloc_top
      |> Syntax.file pkg imports

(* Assertions *)

let assert_file_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.File expected, Syntax.File actual ->
    ImportTest.assert_pkg_equal ~ctxt expected.pkg actual.pkg;
    List.iter2 (ImportTest.assert_import_equal ~ctxt) expected.imports actual.imports;
    List.iter2 (AstTest.assert_top_equal ~ctxt) expected.tops actual.tops

(* Tests *)

(* Constructors *)

let test_file ctxt =
  let seq = Sym.seq () in
  let pkg =
    let id = ImportTest.fresh_name ~seq () in
    ImportTest.fresh_pkg ~id ()
  in

  let imports = [] in
  let tops = [] in
  match Syntax.file pkg imports tops with
    | Syntax.File file ->
      ImportTest.assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (ImportTest.assert_import_equal ~ctxt) imports file.imports;
      List.iter2 (AstTest.assert_top_equal ~ctxt) tops file.tops;

  let imports = [
    ImportTest.fresh_import ();
    ImportTest.fresh_import ()
  ] in
  let tops = [
    (* TODO *)
  ] in
  match Syntax.file pkg imports tops with
    | Syntax.File file ->
      ImportTest.assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (ImportTest.assert_import_equal ~ctxt) imports file.imports;
      List.iter2 (AstTest.assert_top_equal ~ctxt) tops file.tops

let test_constructor =
  "Constructor" >:: test_file

(* Pretty Printing *)

let assert_pp_file = PrettyTest.assert_pp Syntax.pp_file

let test_pp_file_no_imports ctxt =
  let id = "testpackage" in
  let name = ImportTest.fresh_name ~id () in

  let pkg = ImportTest.fresh_pkg ~id:name () in
  Syntax.file pkg [] []
    |> assert_pp_file ~ctxt [
         sprintf "package %s" id
       ]

let test_pp_file_with_imports ctxt =
  let id = "testpackage" in
  let src = "testsrc" in
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkg_stmt =
    let id = ImportTest.fresh_name ~id () in
    ImportTest.fresh_pkg ~id ()
  in
  let imports =
    let pkg = ImportTest.fresh_name ~id:pkg () in
    let local = ImportTest.fresh_name ~id:local () in
    let pkg' = ImportTest.fresh_name ~id:pkg' () in

    let pkgs =
      Syntax.pkgs LocTest.dummy [
        ImportTest.fresh_alias ~pkg ~local ();
        ImportTest.fresh_alias ~pkg:pkg' ()
      ]
    in

    let import =
      let from =
        let name = ImportTest.fresh_name ~id:src () in
        let src = ImportTest.fresh_src ~name () in
        ImportTest.fresh_from ~src ()
      in

      ImportTest.fresh_import ~stdlib:false ~from ~pkgs ()
    in
    let import' = ImportTest.fresh_import ~pkgs () in
    [import; import']
  in
  Syntax.file pkg_stmt imports []
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

let test_pp =
  "Pretty Printing" >::: [
    "With Imports" >:: test_pp_file_with_imports;
    "No Imports"   >:: test_pp_file_no_imports;
  ]

(* Test Suite *)

let suite =
  "Source Files" >::: [
    test_constructor;
    test_pp;
  ]
