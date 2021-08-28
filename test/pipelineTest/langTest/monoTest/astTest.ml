open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Mono.pp_atom
(* let expr_not_equal = TestUtils.not_equal "Expressions" Mono.pp_expr *)
(* let block_not_equal = TestUtils.not_equal "Block values" Mono.pp_block *)
let patt_not_equal = TestUtils.not_equal "Patterns" Mono.pp_patt
(* let binding_not_equal = TestUtils.not_equal "Bindings" Mono.pp_binding *)
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Mono.pp_top *)

let assert_atom_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.Bool expected, Mono.Bool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Mono.Int expected, Mono.Int actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Mono.Long expected, Mono.Long actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Mono.Float expected, Mono.Float actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Mono.Double expected, Mono.Double actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Mono.Rune expected, Mono.Rune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Mono.String expected, Mono.String actual ->
    let cmp s s' = List.fold_left2 (fun acc c c' -> acc && Uchar.equal c c') true s s' in
    let printer s =
      s
        |> List.map Uchar.to_char
        |> List.map (sprintf "%c")
        |> String.concat ""
    in
    assert_equal ~ctxt ~cmp ~printer ~msg:"String values are not equal" expected.value actual.value
  | Mono.Ident expected, Mono.Ident actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> atom_not_equal ~ctxt expected actual

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.Atom expected, Mono.Atom actual ->
    assert_atom_equal ~ctxt expected.atom actual.atom

let assert_block_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.Expr expected, Mono.Expr actual ->
    assert_expr_equal ~ctxt expected.expr actual.expr

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.PattGround, Mono.PattGround -> ()
  | Mono.PattVar expected, Mono.PattVar actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.Binding expected, Mono.Binding actual ->
    assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.Let expected, Mono.Let actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

(* Atoms *)

let test_atom_bool ctxt =
  let value = true in
  let expected = Mono.atom_bool value in
  match expected with
    | Mono.Bool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_int ctxt =
  let value = 42l in
  let expected = Mono.atom_int value in
  match expected with
    | Mono.Int actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_long ctxt =
  let value = 42L in
  let expected = Mono.atom_long value in
  match expected with
    | Mono.Long actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_float ctxt =
  let value = 4.2 in
  let expected = Mono.atom_float value in
  match expected with
    | Mono.Float actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_double ctxt =
  let value = 4.2 in
  let expected = Mono.atom_double value in
  match expected with
    | Mono.Double actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Mono.atom_rune value in
  match expected with
    | Mono.Rune actual ->
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
  let expected = Mono.atom_string value in
  match expected with
    | Mono.String actual ->
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
  let expected = Mono.atom_ident id in
  match expected with
    | Mono.Ident actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> atom_not_equal ~ctxt expected actual

(* Expressions *)

let test_expr_atom ctxt =
  let atom = Mono.atom_bool true in
  let expected = Mono.expr_atom atom in
  match expected with
    | Mono.Atom actual ->
      assert_atom_equal ~ctxt atom actual.atom

(* Blocks *)

let test_block_expr ctxt =
  let expr =
    Mono.atom_bool true
      |> Mono.expr_atom
  in
  let expected = Mono.block_expr expr in
  match expected with
    | Mono.Expr actual ->
      assert_expr_equal ~ctxt expr actual.expr

(* Patterns *)

let test_patt_ground ctxt =
  let expected = Mono.patt_ground in
  match expected with
    | Mono.PattGround -> ()
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Mono.patt_var id in
  match expected with
    | Mono.PattVar actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = Mono.patt_ground in
  let ty = Mono.ty_bool in
  let value =
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
  in
  let expected = Mono.binding patt ty value in
  match expected with
    | Mono.Binding actual ->
      assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding =
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding Mono.patt_ground Mono.ty_bool
  in
  let expected = Mono.top_let binding in
  match expected with
    | Mono.Let actual ->
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
