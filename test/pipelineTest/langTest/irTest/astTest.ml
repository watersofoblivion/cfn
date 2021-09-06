open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Ir.pp_atom
let expr_not_equal = TestUtils.not_equal "Expressions" Ir.pp_expr
let block_not_equal = TestUtils.not_equal "Block values" Ir.pp_block
let patt_not_equal = TestUtils.not_equal "Patterns" Ir.pp_patt
(* let binding_not_equal = TestUtils.not_equal "Bindings" Ir.pp_binding *)
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Ir.pp_top *)

let assert_atom_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.AtomBool expected, Ir.AtomBool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Ir.AtomInt expected, Ir.AtomInt actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Ir.AtomLong expected, Ir.AtomLong actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Ir.AtomFloat expected, Ir.AtomFloat actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Ir.AtomDouble expected, Ir.AtomDouble actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Ir.AtomRune expected, Ir.AtomRune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Ir.AtomString expected, Ir.AtomString actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" expected.value actual.value
  | Ir.AtomIdent expected, Ir.AtomIdent actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> atom_not_equal ~ctxt expected actual

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.ExprBuiltin expected, Ir.ExprBuiltin actual ->
    BuiltinTest.assert_builtin_equal ~ctxt expected.fn actual.fn;
    List.iter2 (assert_atom_equal ~ctxt) expected.args actual.args
  | Ir.ExprAtom expected, Ir.ExprAtom actual ->
    assert_atom_equal ~ctxt expected.atom actual.atom
  | expected, actual -> expr_not_equal ~ctxt expected actual

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.PattGround, Ir.PattGround -> ()
  | Ir.PattVar expected, Ir.PattVar actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.Binding expected, Ir.Binding actual ->
    assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let rec assert_block_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.BlockLet expected, Ir.BlockLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding;
    assert_block_equal ~ctxt expected.scope actual.scope
  | Ir.BlockExpr expected, Ir.BlockExpr actual ->
    assert_expr_equal ~ctxt expected.expr actual.expr
  | expected, actual -> block_not_equal ~ctxt expected actual

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.TopLet expected, Ir.TopLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

(* Atoms *)

let test_atom_bool ctxt =
  let value = true in
  let expected = Ir.atom_bool value in
  match expected with
    | Ir.AtomBool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_int ctxt =
  let value = 42l in
  let expected = Ir.atom_int value in
  match expected with
    | Ir.AtomInt actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_long ctxt =
  let value = 42L in
  let expected = Ir.atom_long value in
  match expected with
    | Ir.AtomLong actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_float ctxt =
  let value = 4.2 in
  let expected = Ir.atom_float value in
  match expected with
    | Ir.AtomFloat actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_double ctxt =
  let value = 4.2 in
  let expected = Ir.atom_double value in
  match expected with
    | Ir.AtomDouble actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Ir.atom_rune value in
  match expected with
    | Ir.AtomRune actual ->
      let printer c = sprintf "%c" (Uchar.to_char c) in
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_string ctxt =
  let value = "foobar" in
  let expected = Ir.atom_string value in
  match expected with
    | Ir.AtomString actual ->
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_ident ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Ir.atom_ident id in
  match expected with
    | Ir.AtomIdent actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> atom_not_equal ~ctxt expected actual

(* Expressions *)

let test_expr_atom ctxt =
  let atom = Ir.atom_bool true in
  let expected = Ir.expr_atom atom in
  match expected with
    | Ir.ExprAtom actual ->
      assert_atom_equal ~ctxt atom actual.atom
    | actual -> expr_not_equal ~ctxt expected actual

(* Blocks *)

let test_block_expr ctxt =
  let expr =
    Ir.atom_bool true
      |> Ir.expr_atom
  in
  let expected = Ir.block_expr expr in
  match expected with
    | Ir.BlockExpr actual ->
      assert_expr_equal ~ctxt expr actual.expr
    | actual -> block_not_equal ~ctxt expected actual

(* Patterns *)

let test_patt_ground ctxt =
  let expected = Ir.patt_ground in
  match expected with
    | Ir.PattGround -> ()
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Ir.patt_var id in
  match expected with
    | Ir.PattVar actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = Ir.patt_ground in
  let ty = Ir.ty_bool in
  let value =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
  in
  let expected = Ir.binding patt ty value in
  match expected with
    | Ir.Binding actual ->
      assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding Ir.patt_ground Ir.ty_bool
  in
  let expected = Ir.top_let binding in
  match expected with
    | Ir.TopLet actual ->
      assert_binding_equal ~ctxt binding actual.binding

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    "Constructors" >::: [
      "Atoms" >::: [
        "Booleans"    >:: test_atom_bool;
        "Integers"    >:: test_atom_int;
        "Longs"       >:: test_atom_long;
        "Floats"      >:: test_atom_float;
        "Doubles"     >:: test_atom_double;
        "Runes"       >:: test_atom_rune;
        "Strings"     >:: test_atom_string;
        "Identifiers" >:: test_atom_ident;
      ];
      "Expressions" >::: [
        "Atoms" >:: test_expr_atom;
      ];
      "Blocks" >::: [
        "Expressions" >:: test_block_expr;
      ];
      "Patterns" >::: [
        "Ground"    >:: test_patt_ground;
        "Variables" >:: test_patt_var;
      ];
      "Bindings" >:: test_binding;
      "Top-Level Expressions" >::: [
        "Let Bindings" >:: test_top_let;
      ];
    ];
  ]
