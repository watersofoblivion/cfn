(* Source Files *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let pkg_lexeme = "testpackage"
let src_lexeme = "testsrc"
let import_lexeme = "importedpkg"
let alias_lexeme = "pkgalias"
let import_lexeme' = "anotherpkg"

let let_lexeme = "letBound"
let let_ty = Prim.id_bool
let let_value = true

let val_lexeme = "ValBound"
let val_ty = Prim.id_int
let val_value = 42l

let pkg =
  let id = ImportTest.fresh_name ~id:pkg_lexeme () in
  ImportTest.fresh_pkg ~id ()

let from =
  let name = ImportTest.fresh_name ~id:src_lexeme () in
  let src = ImportTest.fresh_src ~name () in
  ImportTest.fresh_from ~src ()

let alias =
  let pkg = ImportTest.fresh_name ~id:import_lexeme () in
  let local = ImportTest.fresh_name ~id:alias_lexeme () in
  ImportTest.fresh_alias ~pkg ~alias:true ~local ()

let alias' =
  let pkg = ImportTest.fresh_name ~id:import_lexeme' () in
  ImportTest.fresh_alias ~pkg ()

let pkgs =
  ImportTest.fresh_pkgs ~aliases:[alias; alias'] ()

let ext_import =
  ImportTest.fresh_import ~stdlib:false ~from ~pkgs ()

let stdlib_import =
  ImportTest.fresh_import ~pkgs ()

let imports =
  [ext_import; stdlib_import]

let top_let =
  let binding =
    let patt =
      let id = SymTest.fresh_sym ~id:let_lexeme () in
      PattTest.fresh_patt_var ~id ()
    in
    let ty =
      let id = SymTest.fresh_sym ~id:let_ty () in
      TypeTest.fresh_ty_constr ~id ()
    in
    let value = AstTest.fresh_expr_bool ~value:let_value () in
    AstTest.fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  AstTest.fresh_top_let ~binding ()

let top_val =
  let binding =
    let patt =
      let id = SymTest.fresh_sym ~id:val_lexeme () in
      PattTest.fresh_patt_var ~id ()
    in
    let ty =
      let id = SymTest.fresh_sym ~id:val_ty () in
      TypeTest.fresh_ty_constr ~id ()
    in
    let value = AstTest.fresh_expr_int ~value:val_value () in
    AstTest.fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  AstTest.fresh_top_val ~binding ()

let tops =
  [top_let; top_val]

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
  match Syntax.file pkg imports tops with
    | Syntax.File file ->
      ImportTest.assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (ImportTest.assert_import_equal ~ctxt) imports file.imports;
      List.iter2 (AstTest.assert_top_equal ~ctxt) tops file.tops

let test_constructor =
  "Constructor" >:: test_file

(* Pretty Printing *)

let assert_pp_file = PrettyTest.assert_pp Syntax.pp_file

let test_pp_file_package_only ctxt =
  Syntax.file pkg [] []
    |> assert_pp_file ~ctxt [
         sprintf "package %s" pkg_lexeme;
       ]

let test_pp_file_imports ctxt =
  Syntax.file pkg imports []
    |> assert_pp_file ~ctxt [
         sprintf "package %s" pkg_lexeme;
         "";
         sprintf "from %s import" src_lexeme;
         sprintf "                   | %s -> %s" import_lexeme alias_lexeme;
         sprintf "                   | %s" import_lexeme';
         sprintf "import";
         sprintf "      | %s -> %s" import_lexeme alias_lexeme;
         sprintf "      | %s" import_lexeme';
       ]

let test_pp_file_tops ctxt =
  Syntax.file pkg [] tops
    |> assert_pp_file ~ctxt [
         sprintf "package %s" pkg_lexeme;
         "";
         sprintf "let %s: %s = %b" let_lexeme let_ty let_value;
         "";
         sprintf "val %s: %s = %ld" val_lexeme val_ty val_value;
       ]

let test_pp_file_imports_tops ctxt =
  Syntax.file pkg imports tops
    |> assert_pp_file ~ctxt [
         sprintf "package %s" pkg_lexeme;
         "";
         sprintf "from %s import" src_lexeme;
         sprintf "                   | %s -> %s" import_lexeme alias_lexeme;
         sprintf "                   | %s" import_lexeme';
         sprintf "import";
         sprintf "      | %s -> %s" import_lexeme alias_lexeme;
         sprintf "      | %s" import_lexeme';
         "";
         sprintf "let %s: %s = %b" let_lexeme let_ty let_value;
         "";
         sprintf "val %s: %s = %ld" val_lexeme val_ty val_value;
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Package Only"                      >:: test_pp_file_package_only;
    "Imports"                           >:: test_pp_file_imports;
    "Top-Level Expressions"             >:: test_pp_file_tops;
    "Imports and Top-Level Expressions" >:: test_pp_file_imports_tops;
  ]

(* Test Suite *)

let suite =
  "Source Files" >::: [
    test_constructor;
    test_pp;
  ]
