open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_ty_constr ?seq:(seq = Sym.seq ()) ?id:(id = Prim.id_bool) _ =
  let loc = LocTest.gen () in
  seq
    |> Sym.gen ~id
    |> Syntax.ty_constr loc

let fresh_expr_bool ?value:(value = true) _ =
  let loc = LocTest.gen () in
  Syntax.expr_bool loc value

let fresh_expr_int ?value:(value = 42l) _ =
  let loc = LocTest.gen () in
  value
    |> Int32.to_string
    |> Syntax.expr_int loc

let fresh_expr_long ?value:(value = 42L) _ =
  let loc = LocTest.gen () in
  value
    |> Int64.to_string
    |> Syntax.expr_long loc

let fresh_expr_float ?value:(value = 4.2) _ =
  let loc = LocTest.gen () in
  value
    |> sprintf "%g"
    |> Syntax.expr_float loc

let fresh_expr_double ?value:(value = 4.2) _ =
  let loc = LocTest.gen () in
  value
    |> sprintf "%g"
    |> Syntax.expr_double loc

let fresh_expr_rune ?value:(value = 'a') _ =
  let loc = LocTest.gen () in
  value
    |> Uchar.of_char
    |> Syntax.expr_rune loc

let fresh_expr_string ?value:(value = "foo bar") _ =
  let loc = LocTest.gen () in
  value
    |> String.to_seq
    |> List.of_seq
    |> List.map Uchar.of_char
    |> Syntax.expr_string loc

let fresh_expr_ident ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  seq
    |> Sym.gen ~id
    |> Syntax.expr_ident loc

let fresh_patt_ground _ =
  ()
    |> LocTest.gen
    |> Syntax.patt_ground

let fresh_patt_var ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  seq
    |> Sym.gen ~id
    |> Syntax.patt_var loc

let fresh_value_binding ?explicit:(explicit = false) ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  let patt = fresh_patt_var ~seq ~id () in
  let ty =
    if explicit
    then Some (fresh_ty_constr ~seq ())
    else None
  in
  ()
    |> fresh_expr_bool
    |> Syntax.value_binding loc patt ty

let fresh_name ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  seq
    |> Sym.gen ~id
    |> Syntax.name loc

let fresh_src ?seq:(seq = Sym.seq ()) ?name:(name = "") _ =
  let loc = LocTest.gen () in
  ()
    |> fresh_name ~seq ~id:name
    |> Syntax.src loc

let fresh_from ?seq:(seq = Sym.seq ()) ?src:(src = "") _ =
  let loc = LocTest.gen () in
  ()
    |> fresh_src ~seq ~name:src
    |> Syntax.from loc

let fresh_alias ?seq:(seq = Sym.seq ()) ?pkg:(pkg = "") ?local:(local = "") _ =
  let loc = LocTest.gen () in
  let pkg = fresh_name ~seq ~id:pkg () in
  let local =
    if local <> ""
    then Some (fresh_name ~seq ~id:local ())
    else None
  in
  Syntax.alias loc pkg local

let fresh_pkgs ?seq:(seq = Sym.seq ()) ?local:(local = "localname") _ =
  let loc = LocTest.gen () in
  Syntax.pkgs loc [
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
      Syntax.pkgs loc []
    else fresh_pkgs ~seq ()
  in
  Syntax.import loc from pkgs

let fresh_pkg ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  let loc = LocTest.gen () in
  fresh_name ~seq ~id ()
    |> Syntax.pkg loc

(* Utilities *)

(* Location Stripping *)

let deloc_optional deloc = function
  | Some value -> Some (deloc value)
  | None -> None

let deloc_ty = function
  | Syntax.TyConstr constr -> Syntax.ty_constr LocTest.dummy constr.id

let deloc_expr = function
  | Syntax.Bool b -> Syntax.expr_bool LocTest.dummy b.value
  | Syntax.Int i -> Syntax.expr_int LocTest.dummy i.lexeme
  | Syntax.Long l -> Syntax.expr_long LocTest.dummy l.lexeme
  | Syntax.Float f -> Syntax.expr_float LocTest.dummy f.lexeme
  | Syntax.Double d -> Syntax.expr_double LocTest.dummy d.lexeme
  | Syntax.Rune r -> Syntax.expr_rune LocTest.dummy r.value
  | Syntax.String s -> Syntax.expr_string LocTest.dummy s.value
  | Syntax.Ident ident -> Syntax.expr_ident LocTest.dummy ident.id

let deloc_patt = function
  | Syntax.PattGround _ -> Syntax.patt_ground LocTest.dummy
  | Syntax.PattVar patt -> Syntax.patt_var LocTest.dummy patt.id

let deloc_binding = function
  | Syntax.ValueBinding binding ->
    let patt = deloc_patt binding.patt in
    let ty = deloc_optional deloc_ty binding.ty in
    binding.value
      |> deloc_expr
      |> Syntax.value_binding LocTest.dummy patt ty

let deloc_top = function
  | Syntax.Let top ->
    top.binding
      |> deloc_binding
      |> Syntax.top_let LocTest.dummy
  | Syntax.Val top ->
    top.binding
      |> deloc_binding
      |> Syntax.top_val LocTest.dummy

let deloc_name = function
  | Syntax.Name name -> Syntax.name LocTest.dummy name.id

let deloc_src = function
  | Syntax.Source source ->
    source.name
      |> deloc_name
      |> Syntax.src LocTest.dummy

let deloc_from = function
  | Syntax.From from ->
    from.src
      |> deloc_src
      |> Syntax.from LocTest.dummy

let deloc_alias = function
  | Syntax.Alias alias ->
    let pkg = deloc_name alias.pkg in
    alias.alias
      |> deloc_optional deloc_name
      |> Syntax.alias LocTest.dummy pkg

let deloc_pkgs = function
  | Syntax.Packages pkgs ->
    pkgs.pkgs
      |> List.map deloc_alias
      |> Syntax.pkgs LocTest.dummy

let deloc_import = function
  | Syntax.Import import ->
    let from = deloc_optional deloc_from import.from in
    import.pkgs
      |> deloc_pkgs
      |> Syntax.import LocTest.dummy from

let deloc_pkg = function
  | Syntax.Package pkg ->
    pkg.id
      |> deloc_name
      |> Syntax.pkg LocTest.dummy

let deloc_file = function
  | Syntax.File file ->
    let pkg = deloc_pkg file.pkg in
    let imports =
      file.imports
        |> List.map deloc_import
    in
    file.tops
      |> List.map deloc_top
      |> Syntax.file pkg imports

(* Assertions *)

let expr_not_equal = TestUtils.not_equal "Expressions" Syntax.pp_expr
let patt_not_equal = TestUtils.not_equal "Patterns" Syntax.pp_patt
let top_not_equal = TestUtils.not_equal "Top-level expressions" Syntax.pp_top

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Bool expected, Syntax.Bool actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Syntax.Int expected, Syntax.Int actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Integer lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.Long expected, Syntax.Long actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Long lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.Float expected, Syntax.Float actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Float lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.Double expected, Syntax.Double actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Double lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.Rune expected, Syntax.Rune actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    let printer r = sprintf "%c" (Uchar.to_char r) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Syntax.String expected, Syntax.String actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    let printer r = sprintf "%c" (Uchar.to_char r) in
    List.iter2 (assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"String values are not equal") expected.value actual.value
  | Syntax.Ident expected, Syntax.Ident actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> expr_not_equal ~ctxt expected actual

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.PattGround expected, Syntax.PattGround actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.PattVar expected, Syntax.PattVar actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.ValueBinding expected, Syntax.ValueBinding actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_patt_equal ~ctxt expected.patt actual.patt;
    TestUtils.assert_optional_equal ~ctxt "type" TypeTest.assert_ty_equal expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Let expected, Syntax.Let actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_binding_equal ~ctxt expected.binding actual.binding
  | Syntax.Val expected, Syntax.Val actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_binding_equal ~ctxt expected.binding actual.binding
  | expected, actual -> top_not_equal ~ctxt expected actual

let assert_name_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Name expected, Syntax.Name actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id

let assert_src_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Source expected, Syntax.Source actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.name actual.name

let assert_from_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.From expected, Syntax.From actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_src_equal ~ctxt expected.src actual.src

let assert_alias_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Alias expected, Syntax.Alias actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.pkg actual.pkg;
    TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal expected.alias actual.alias

let assert_pkgs_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Packages expected, Syntax.Packages actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_alias_equal ~ctxt) expected.pkgs actual.pkgs

let assert_import_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Import expected, Syntax.Import actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal expected.from actual.from;
    assert_pkgs_equal ~ctxt expected.pkgs actual.pkgs

let assert_pkg_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Package expected, Syntax.Package actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.id actual.id

let assert_file_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.File expected, Syntax.File actual ->
    assert_pkg_equal ~ctxt expected.pkg actual.pkg;
    List.iter2 (assert_import_equal ~ctxt) expected.imports actual.imports

(* Constructors *)

let test_expr_bool ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.expr_bool loc true in
  match expected with
    | Syntax.Bool actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Boolean values are not equal" ~printer:string_of_bool true actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_int ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+42i" in
  let expected = Syntax.expr_int loc lexeme in
  match expected with
    | Syntax.Int actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Int lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_long ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+42L" in
  let expected = Syntax.expr_long loc lexeme in
  match expected with
    | Syntax.Long actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Long lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_float ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+1.2e-3.4f" in
  let expected = Syntax.expr_float loc lexeme in
  match expected with
    | Syntax.Float actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Float lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_double ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+1.2e-3.4D" in
  let expected = Syntax.expr_double loc lexeme in
  match expected with
    | Syntax.Double actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Double lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme;
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_rune ctxt =
  let loc = LocTest.gen () in
  let value = Uchar.of_char 'a' in
  let expected = Syntax.expr_rune loc value in
  match expected with
    | Syntax.Rune actual ->
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
  let expected = Syntax.expr_string loc value in
  match expected with
    | Syntax.String actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      let cmp s s' = List.fold_left2 (fun acc c c' -> acc && Uchar.equal c c') true s s' in
      let printer s =
        s
          |> List.map (fun c -> sprintf "%c" (Uchar.to_char c))
          |> String.concat ""
      in
      assert_equal ~cmp ~printer ~msg:"String values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_ident ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Syntax.expr_ident loc id in
  match expected with
    | Syntax.Ident actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> expr_not_equal ~ctxt expected actual

let test_patt_ground ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.patt_ground loc in
  match expected with
    | Syntax.PattGround actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Syntax.patt_var loc id in
  match expected with
    | Syntax.PattVar actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

let test_binding_value_binding ctxt =
  let patt = fresh_patt_ground () in
  let value = fresh_expr_bool () in
  let assert_binding_equal ty =
    let loc = LocTest.gen () in
    let expected = Syntax.value_binding loc patt ty value in
    match expected with
      | Syntax.ValueBinding actual ->
        LocTest.assert_loc_equal ~ctxt loc actual.loc;
        assert_patt_equal ~ctxt patt actual.patt;
        TestUtils.assert_optional_equal ~ctxt "Type" TypeTest.assert_ty_equal ty actual.ty;
        assert_expr_equal ~ctxt value actual.value
  in
  assert_binding_equal None;
  assert_binding_equal (Some (fresh_ty_constr ()))

let test_top_let ctxt =
  let loc = LocTest.gen () in
  let binding = fresh_value_binding () in
  let expected = Syntax.top_let loc binding in
  match expected with
    | Syntax.Let actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_binding_equal ~ctxt binding actual.binding
    | actual -> top_not_equal ~ctxt expected actual

let test_top_val ctxt =
  let loc = LocTest.gen () in
  let binding = fresh_value_binding () in
  let expected = Syntax.top_val loc binding in
  match expected with
    | Syntax.Val actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_binding_equal ~ctxt binding actual.binding
    | actual -> top_not_equal ~ctxt expected actual

let test_name ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  match Syntax.name loc id with
    | Syntax.Name name ->
      LocTest.assert_loc_equal ~ctxt loc name.loc;
      SymTest.assert_sym_equal ~ctxt id name.id

let test_src ctxt =
  let loc = LocTest.gen () in
  let name = fresh_name () in
  match Syntax.src loc name with
    | Syntax.Source src ->
      LocTest.assert_loc_equal ~ctxt loc src.loc;
      assert_name_equal ~ctxt name src.name

let test_from ctxt =
  let loc = LocTest.gen () in
  let src = fresh_src () in
  match Syntax.from loc src with
    | Syntax.From from ->
      LocTest.assert_loc_equal ~ctxt loc from.loc;
      assert_src_equal ~ctxt src from.src

let test_alias ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let pkg = fresh_name ~seq () in

  let local = None in
  match Syntax.alias loc pkg local with
    | Syntax.Alias alias ->
      LocTest.assert_loc_equal ~ctxt loc alias.loc;
      assert_name_equal ~ctxt pkg alias.pkg;
      TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal local alias.alias;

  let local = Some (fresh_name ~seq ()) in
  match Syntax.alias loc pkg local with
    | Syntax.Alias alias ->
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
  match Syntax.pkgs loc aliases with
    | Syntax.Packages pkgs ->
      LocTest.assert_loc_equal ~ctxt loc pkgs.loc;
      List.iter2 (assert_alias_equal ~ctxt) aliases pkgs.pkgs

let test_import ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in

  let from = None in
  let pkgs = Syntax.pkgs LocTest.dummy [] in
  match Syntax.import loc from pkgs with
    | Syntax.Import import ->
      LocTest.assert_loc_equal ~ctxt loc import.loc;
      TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
      assert_pkgs_equal ~ctxt pkgs import.pkgs;

  let from = Some (fresh_from ~seq ()) in
  let pkgs = fresh_pkgs ~seq () in
  match Syntax.import loc from pkgs with
    | Syntax.Import import ->
      LocTest.assert_loc_equal ~ctxt loc import.loc;
      TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
      assert_pkgs_equal ~ctxt pkgs import.pkgs

let test_pkg ctxt =
  let loc = LocTest.gen () in
  let id = fresh_name () in
  match Syntax.pkg loc id with
    | Syntax.Package pkg ->
      LocTest.assert_loc_equal ~ctxt loc pkg.loc;
      assert_name_equal ~ctxt id pkg.id

let test_file ctxt =
  let seq = Sym.seq () in
  let pkg = fresh_pkg ~seq () in

  let imports = [] in
  let tops = [] in
  match Syntax.file pkg imports tops with
    | Syntax.File file ->
      assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (assert_import_equal ~ctxt) imports file.imports;
      List.iter2 (assert_top_equal ~ctxt) tops file.tops;

  let imports = [
    fresh_import ~seq ~from:false ~pkgs:false ();
    fresh_import ~seq ~from:true ~pkgs:true ()
  ] in
  let tops = [
    (* TODO *)
  ] in
  match Syntax.file pkg imports tops with
    | Syntax.File file ->
      assert_pkg_equal ~ctxt pkg file.pkg;
      List.iter2 (assert_import_equal ~ctxt) imports file.imports;
      List.iter2 (assert_top_equal ~ctxt) tops file.tops

let test_constructor =
  "Constructors" >::: [
    "Expressions" >::: [
      "Booleans"    >:: test_expr_bool;
      "Integers"    >:: test_expr_int;
      "Longs"       >:: test_expr_long;
      "Floats"      >:: test_expr_float;
      "Doubles"     >:: test_expr_double;
      "Runes"       >:: test_expr_rune;
      "Strings"     >:: test_expr_string;
      "Identifiers" >:: test_expr_ident;
    ];
    "Patterns" >::: [
      "Ground"     >:: test_patt_ground;
      "Identifier" >:: test_patt_var;
    ];
    "Bindings" >:: test_binding_value_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
      "Val Bindings" >:: test_top_val;
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
