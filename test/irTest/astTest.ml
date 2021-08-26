open Format

open OUnit2

open Common
open Ir

open CommonTest

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Fmt.atom
(* let expr_not_equal = TestUtils.not_equal "Expressions" Fmt.expr *)
(* let block_not_equal = TestUtils.not_equal "Block values" Fmt.block *)
let patt_not_equal = TestUtils.not_equal "Patterns" Fmt.patt
(* let binding_not_equal = TestUtils.not_equal "Bindings" Fmt.binding *)
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Fmt.top *)

let assert_atom_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Bool expected, Ast.Bool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Ast.Int expected, Ast.Int actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Ast.Long expected, Ast.Long actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Ast.Float expected, Ast.Float actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Ast.Double expected, Ast.Double actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Ast.Rune expected, Ast.Rune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Ast.String expected, Ast.String actual ->
    let cmp s s' = List.fold_left2 (fun acc c c' -> acc && Uchar.equal c c') true s s' in
    let printer s =
      s
        |> List.map Uchar.to_char
        |> List.map (sprintf "%c")
        |> String.concat ""
    in
    assert_equal ~ctxt ~cmp ~printer ~msg:"String values are not equal" expected.value actual.value
  | Ast.Ident expected, Ast.Ident actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> atom_not_equal ~ctxt expected actual

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Atom expected, Ast.Atom actual ->
    assert_atom_equal ~ctxt expected.atom actual.atom

let assert_block_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Expr expected, Ast.Expr actual ->
    assert_expr_equal ~ctxt expected.expr actual.expr

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.PattGround, Ast.PattGround -> ()
  | Ast.PattVar expected, Ast.PattVar actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Binding expected, Ast.Binding actual ->
    assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Ast.Let expected, Ast.Let actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

(* Atoms *)

let test_atom_bool ctxt =
  let value = true in
  let expected = Ast.atom_bool value in
  match expected with
    | Ast.Bool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_int ctxt =
  let value = 42l in
  let expected = Ast.atom_int value in
  match expected with
    | Ast.Int actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_long ctxt =
  let value = 42L in
  let expected = Ast.atom_long value in
  match expected with
    | Ast.Long actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_float ctxt =
  let value = 4.2 in
  let expected = Ast.atom_float value in
  match expected with
    | Ast.Float actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_double ctxt =
  let value = 4.2 in
  let expected = Ast.atom_double value in
  match expected with
    | Ast.Double actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Ast.atom_rune value in
  match expected with
    | Ast.Rune actual ->
      let printer c = sprintf "%c" (Uchar.to_char c) in
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_string ctxt =
  let value =
    "foobar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let expected = Ast.atom_string value in
  match expected with
    | Ast.String actual ->
      let cmp s s' = List.fold_left2 (fun acc c c' -> acc && Uchar.equal c c') true s s' in
      let printer s =
        s
          |> List.map Uchar.to_char
          |> List.map (sprintf "%c")
          |> String.concat ""
      in
      assert_equal ~ctxt ~cmp ~printer ~msg:"String values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_ident ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Ast.atom_ident id in
  match expected with
    | Ast.Ident actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> atom_not_equal ~ctxt expected actual

(* Expressions *)

let test_expr_atom ctxt =
  let atom = Ast.atom_bool true in
  let expected = Ast.expr_atom atom in
  match expected with
    | Ast.Atom actual ->
      assert_atom_equal ~ctxt atom actual.atom

(* Blocks *)

let test_block_expr ctxt =
  let expr =
    Ast.atom_bool true
      |> Ast.expr_atom
  in
  let expected = Ast.block_expr expr in
  match expected with
    | Ast.Expr actual ->
      assert_expr_equal ~ctxt expr actual.expr

(* Patterns *)

let test_patt_ground ctxt =
  let expected = Ast.patt_ground in
  match expected with
    | Ast.PattGround -> ()
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Ast.patt_var id in
  match expected with
    | Ast.PattVar actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = Ast.patt_ground in
  let ty = Type.bool in
  let value =
    true
      |> Ast.atom_bool
      |> Ast.expr_atom
  in
  let expected = Ast.binding patt ty value in
  match expected with
    | Ast.Binding actual ->
      assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding =
    true
      |> Ast.atom_bool
      |> Ast.expr_atom
      |> Ast.binding Ast.patt_ground Type.bool
  in
  let expected = Ast.top_let binding in
  match expected with
    | Ast.Let actual ->
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
