open Format

open OUnit2

open Common
open Syntax

open CommonTest

(* Fixtures *)

let fresh_name ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  seq
    |> Sym.gen ~id
    |> Ast.name loc

let fresh_src ?seq:(seq = Sym.seq ()) ?name:(name = "") _ =
  let loc = LocTest.gen () in
  ()
    |> fresh_name ~seq ~id:name
    |> Ast.src loc

let fresh_from ?seq:(seq = Sym.seq ()) ?src:(src = "") _ =
  let loc = LocTest.gen () in
  ()
    |> fresh_src ~seq ~name:src
    |> Ast.from loc

let fresh_alias ?seq:(seq = Sym.seq ()) ?pkg:(pkg = "") ?local:(local = "") _ =
  let loc = LocTest.gen () in
  let pkg = fresh_name ~seq ~id:pkg () in
  let local =
    if local <> ""
    then Some (fresh_name ~seq ~id:local ())
    else None
  in
  Ast.alias loc pkg local

let fresh_pkgs ?seq:(seq = Sym.seq ()) ?local:(local = "localname") _ =
  let loc = LocTest.gen () in
  Ast.pkgs loc [
    fresh_alias ~seq ();
    fresh_alias ~seq ~local ()
  ]

let fresh_import ?seq:(seq = Sym.seq ()) ?from:(from = false) ?pkgs:(pkgs = false) _ =
  let loc = LocTest.gen () in
  let from =
    if from
    then Some (fresh_from ~seq ())
    else None
  in
  let pkgs =
    if pkgs
    then
      let loc = LocTest.gen () in
      Ast.pkgs loc []
    else fresh_pkgs ~seq ()
  in
  Ast.import loc from pkgs

let fresh_pkg ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  fresh_name ~seq ~id ()
    |> Ast.pkg loc

(* Utilities *)

(* Location Stripping *)

let deloc_optional deloc = function
  | Some value -> Some (deloc value)
  | None -> None

let deloc_expr = function
  | Ast.Bool b -> Ast.bool LocTest.dummy b.value
  | Ast.Int i -> Ast.int LocTest.dummy i.lexeme
  | Ast.Long l -> Ast.long LocTest.dummy l.lexeme
  | Ast.Float f -> Ast.float LocTest.dummy f.lexeme
  | Ast.Double d -> Ast.double LocTest.dummy d.lexeme
  | Ast.Rune r -> Ast.rune LocTest.dummy r.value
  | Ast.String s -> Ast.string LocTest.dummy s.value

let deloc_name = function
  | Ast.Name name -> Ast.name LocTest.dummy name.id

let deloc_src = function
  | Ast.Source source ->
    source.name
      |> deloc_name
      |> Ast.src LocTest.dummy

let deloc_from = function
  | Ast.From from ->
    from.src
      |> deloc_src
      |> Ast.from LocTest.dummy

let deloc_alias = function
  | Ast.Alias alias ->
    let pkg = deloc_name alias.pkg in
    alias.alias
      |> deloc_optional deloc_name
      |> Ast.alias LocTest.dummy pkg

let deloc_pkgs = function
  | Ast.Packages pkgs ->
    pkgs.pkgs
      |> List.map deloc_alias
      |> Ast.pkgs LocTest.dummy

let deloc_import = function
  | Ast.Import import ->
    let from = deloc_optional deloc_from import.from in
    import.pkgs
      |> deloc_pkgs
      |> Ast.import LocTest.dummy from

let deloc_pkg = function
  | Ast.Package pkg ->
    pkg.id
      |> deloc_name
      |> Ast.pkg LocTest.dummy

let deloc_file = function
  | Ast.File file ->
    let pkg = deloc_pkg file.pkg in
    file.imports
      |> List.map deloc_import
      |> Ast.file pkg

(* Assertions *)

let expr_not_equal = TestUtils.not_equal "Expressions" Fmt.expr

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Bool expected, Ast.Bool actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Ast.Int expected, Ast.Int actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Integer lexemes are not equal" expected.lexeme actual.lexeme
  | Ast.Long expected, Ast.Long actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Long lexemes are not equal" expected.lexeme actual.lexeme
  | Ast.Float expected, Ast.Float actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Float lexemes are not equal" expected.lexeme actual.lexeme
  | Ast.Double expected, Ast.Double actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Double lexemes are not equal" expected.lexeme actual.lexeme
  | Ast.Rune expected, Ast.Rune actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    let printer r = sprintf "%c" (Uchar.to_char r) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Ast.String expected, Ast.String actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    let printer r = sprintf "%c" (Uchar.to_char r) in
    List.iter2 (assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"String values are not equal") expected.value actual.value
  | expected, actual -> expr_not_equal ~ctxt expected actual

let assert_name_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Name expected, Ast.Name actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id

let assert_src_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Source expected, Ast.Source actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.name actual.name

let assert_from_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.From expected, Ast.From actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_src_equal ~ctxt expected.src actual.src

let assert_alias_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Alias expected, Ast.Alias actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.pkg actual.pkg;
    TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal expected.alias actual.alias

let assert_pkgs_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Packages expected, Ast.Packages actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_alias_equal ~ctxt) expected.pkgs actual.pkgs

let assert_import_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Import expected, Ast.Import actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal expected.from actual.from;
    assert_pkgs_equal ~ctxt expected.pkgs actual.pkgs

let assert_pkg_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Package expected, Ast.Package actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.id actual.id

let assert_file_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.File expected, Ast.File actual ->
    assert_pkg_equal ~ctxt expected.pkg actual.pkg;
    List.iter2 (assert_import_equal ~ctxt) expected.imports actual.imports

(* Constructors *)

let test_expr_bool ctxt =
  let loc = LocTest.gen () in
  let expected = Ast.bool loc true in
  match expected with
    | Ast.Bool actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Boolean values are not equal" ~printer:string_of_bool true actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_int ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+42i" in
  let expected = Ast.int loc lexeme in
  match expected with
    | Ast.Int actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Int lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_long ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+42L" in
  let expected = Ast.long loc lexeme in
  match expected with
    | Ast.Long actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Long lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_float ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+1.2e-3.4f" in
  let expected = Ast.float loc lexeme in
  match expected with
    | Ast.Float actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Float lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_double ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+1.2e-3.4D" in
  let expected = Ast.double loc lexeme in
  match expected with
    | Ast.Double actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Double lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_rune ctxt =
  let loc = LocTest.gen () in
  let value = Uchar.of_char 'a' in
  let expected = Ast.rune loc value in
  match expected with
    | Ast.Rune actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      let printer r = sprintf "%c" (Uchar.to_char r) in
      assert_equal ~cmp:Uchar.equal ~msg:"Rune values are not equal" ~printer value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_string ctxt =
  let loc = LocTest.gen () in
  let value =
    "asdf"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let expected = Ast.string loc value in
  match expected with
    | Ast.String actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      let cmp s s' = List.fold_left2 (fun acc c c' -> acc && Uchar.equal c c') true s s' in
      let printer s =
        s
          |> List.map (fun c -> sprintf "%c" (Uchar.to_char c))
          |> String.concat ""
      in
      assert_equal ~cmp ~printer ~msg:"String values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_name ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  match Ast.name loc id with
    | Ast.Name name ->
      LocTest.assert_loc_equal ~ctxt loc name.loc;
      SymTest.assert_sym_equal ~ctxt id name.id

let test_src ctxt =
  let loc = LocTest.gen () in
  let name = fresh_name () in
  match Ast.src loc name with
    | Ast.Source src ->
      LocTest.assert_loc_equal ~ctxt loc src.loc;
      assert_name_equal ~ctxt name src.name

let test_from ctxt =
  let loc = LocTest.gen () in
  let src = fresh_src () in
  match Ast.from loc src with
    | Ast.From from ->
      LocTest.assert_loc_equal ~ctxt loc from.loc;
      assert_src_equal ~ctxt src from.src

let test_alias ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let pkg = fresh_name ~seq () in

  let local = None in
  match Ast.alias loc pkg local with
    | Ast.Alias alias ->
      LocTest.assert_loc_equal ~ctxt loc alias.loc;
      assert_name_equal ~ctxt pkg alias.pkg;
      TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal local alias.alias;

  let local = Some (fresh_name ~seq ()) in
  match Ast.alias loc pkg local with
    | Ast.Alias alias ->
      LocTest.assert_loc_equal ~ctxt loc alias.loc;
      assert_name_equal ~ctxt pkg alias.pkg;
      TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal local alias.alias

let test_pkgs ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let aliases = [
    fresh_alias ~seq ();
    fresh_alias ~seq ~local:"localname" ()
  ] in
  match Ast.pkgs loc aliases with
    | Ast.Packages pkgs ->
      LocTest.assert_loc_equal ~ctxt loc pkgs.loc;
      List.iter2 (assert_alias_equal ~ctxt) aliases pkgs.pkgs

let test_import ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in

  let from = None in
  let pkgs = Ast.pkgs LocTest.dummy [] in
  match Ast.import loc from pkgs with
    | Ast.Import import ->
      LocTest.assert_loc_equal ~ctxt loc import.loc;
      TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
      assert_pkgs_equal ~ctxt pkgs import.pkgs;

  let from = Some (fresh_from ~seq ()) in
  let pkgs = fresh_pkgs ~seq () in
  match Ast.import loc from pkgs with
    | Ast.Import import ->
      LocTest.assert_loc_equal ~ctxt loc import.loc;
      TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
      assert_pkgs_equal ~ctxt pkgs import.pkgs

let test_pkg ctxt =
  let loc = LocTest.gen () in
  let id = fresh_name () in
  match Ast.pkg loc id with
    | Ast.Package pkg ->
      LocTest.assert_loc_equal ~ctxt loc pkg.loc;
      assert_name_equal ~ctxt id pkg.id

let test_file ctxt =
  let seq = Sym.seq () in
  let pkg = fresh_pkg ~seq () in

  let imports = [] in
  match Ast.file pkg imports with
    | Ast.File file ->
      assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (assert_import_equal ~ctxt) imports file.imports;

  let imports = [
    fresh_import ~seq ~from:false ~pkgs:false ();
    fresh_import ~seq ~from:true ~pkgs:true ()
  ] in
  match Ast.file pkg imports with
    | Ast.File file ->
      assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (assert_import_equal ~ctxt) imports file.imports

let test_constructor =
  "Constructors" >::: [
    "Expressions" >::: [
      "Booleans" >:: test_expr_bool;
      "Integers" >:: test_expr_int;
      "Longs"    >:: test_expr_long;
      "Floats"   >:: test_expr_float;
      "Doubles"  >:: test_expr_double;
      "Runes"    >:: test_expr_rune;
      "Strings"  >:: test_expr_string;
    ];
    "Imports" >::: [
      "Names" >:: test_name;
      "Statements" >::: [
        "Sources"          >:: test_src;
        "From Clauses"     >:: test_from;
        "Alias"            >:: test_alias;
        "Package List"     >:: test_pkgs;
        "Import Statement" >:: test_import;
      ]
    ];
    "Package Statement" >:: test_pkg;
    "Source Files"      >:: test_file;
  ]

(* Test Suite *)
let suite =
  "Abstract Syntax" >::: [
    test_constructor;
  ]
