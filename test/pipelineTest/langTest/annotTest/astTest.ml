open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let expr_not_equal = TestUtils.not_equal "Expressions" Annot.pp_expr
let patt_not_equal = TestUtils.not_equal "Patterns" Annot.pp_patt
(* let binding_not_equal = TestUtils.not_equal "Bindings" Fmt.binding *)
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Fmt.top *)

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.ExprBool expected, Annot.ExprBool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Annot.ExprInt expected, Annot.ExprInt actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Annot.ExprLong expected, Annot.ExprLong actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Annot.ExprFloat expected, Annot.ExprFloat actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Annot.ExprDouble expected, Annot.ExprDouble actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Annot.ExprRune expected, Annot.ExprRune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Annot.ExprString expected, Annot.ExprString actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" expected.value actual.value
  | Annot.ExprIdent expected, Annot.ExprIdent actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> expr_not_equal ~ctxt expected actual

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.PattGround, Annot.PattGround -> ()
  | Annot.PattVar expected, Annot.PattVar actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.Binding expected, Annot.Binding actual ->
    assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.TopLet expected, Annot.TopLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

let test_expr_bool ctxt =
  let value = true in
  let expected = Annot.expr_bool value in
  match expected with
    | Annot.ExprBool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_int ctxt =
  let value = 42l in
  let expected = Annot.expr_int value in
  match expected with
    | Annot.ExprInt actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_long ctxt =
  let value = 42L in
  let expected = Annot.expr_long value in
  match expected with
    | Annot.ExprLong actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_float ctxt =
  let value = 4.2 in
  let expected = Annot.expr_float value in
  match expected with
    | Annot.ExprFloat actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_double ctxt =
  let value = 4.2 in
  let expected = Annot.expr_double value in
  match expected with
    | Annot.ExprDouble actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Annot.expr_rune value in
  match expected with
    | Annot.ExprRune actual ->
      let printer c = sprintf "%c" (Uchar.to_char c) in
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_string ctxt =
  let value = "foobar" in
  let expected = Annot.expr_string value in
  match expected with
    | Annot.ExprString actual ->
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_ident ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Annot.expr_ident id in
  match expected with
    | Annot.ExprIdent actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> expr_not_equal ~ctxt expected actual

let test_patt_ground ctxt =
  let expected = Annot.patt_ground in
  match expected with
    | Annot.PattGround -> ()
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Annot.patt_var id in
  match expected with
    | Annot.PattVar actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

let test_binding ctxt =
  let patt = Annot.patt_ground in
  let ty = Annot.ty_bool in
  let value = Annot.expr_bool true in
  let expected = Annot.binding patt ty value in
  match expected with
    | Annot.Binding actual ->
      assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

let test_top_let ctxt =
  let binding =
    true
      |> Annot.expr_bool
      |> Annot.binding Annot.patt_ground Annot.ty_bool
  in
  let expected = Annot.top_let binding in
  match expected with
    | Annot.TopLet actual ->
      assert_binding_equal ~ctxt binding actual.binding

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
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
      "Bindings" >:: test_binding;
      "Top-Level Expressions" >::: [
        "Let Bindings" >:: test_top_let;
      ];
    ];
  ]
