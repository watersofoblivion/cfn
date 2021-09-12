(* Types *)

open OUnit2

open CommonTest

(* Tests *)

let test_norm_ty_bool ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_bool in
  let ir = Ir.ty_bool in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_int ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_int in
  let ir = Ir.ty_int in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_long ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_long in
  let ir = Ir.ty_long in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_float ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_float in
  let ir = Ir.ty_float in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_double ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_double in
  let ir = Ir.ty_double in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_rune ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_rune in
  let ir = Ir.ty_rune in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_string ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_string in
  let ir = Ir.ty_string in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

(* Test Suite *)

let suite =
  "Types" >::: [
    "Boolean" >:: test_norm_ty_bool;
    "Integer" >:: test_norm_ty_int;
    "Long"    >:: test_norm_ty_long;
    "Float"   >:: test_norm_ty_float;
    "Double"  >:: test_norm_ty_double;
    "Rune"    >:: test_norm_ty_rune;
    "String"  >:: test_norm_ty_string;
  ]
