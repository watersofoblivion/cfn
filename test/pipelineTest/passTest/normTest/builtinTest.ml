(* Built-in Functions *)

open OUnit2

open CommonTest

(* Tests *)

let test_norm_builtin_add ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_add Annot.ty_int in
  let ir = Ir.builtin_add Ir.ty_int in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_sub ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_sub Annot.ty_int in
  let ir = Ir.builtin_sub Ir.ty_int in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_mul ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_mul Annot.ty_int in
  let ir = Ir.builtin_mul Ir.ty_int in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_div ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_div Annot.ty_int in
  let ir = Ir.builtin_div Ir.ty_int in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_mod ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_mod Annot.ty_int in
  let ir = Ir.builtin_mod Ir.ty_int in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_exp ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_exp Annot.ty_float in
  let ir = Ir.builtin_exp Ir.ty_float in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_promote ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_promote Annot.ty_int Annot.ty_long in
  let ir = Ir.builtin_promote Ir.ty_int Ir.ty_long in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

let test_norm_builtin_concat ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.builtin_concat Annot.ty_string in
  let ir = Ir.builtin_concat Ir.ty_string in
  Norm.norm_builtin env annot (fun builtin ->
    IrTest.assert_builtin_equal ~ctxt ir builtin)

(* Test Suite *)

let suite =
  "Built-in Functions" >::: [
    "Addition"       >:: test_norm_builtin_add;
    "Subtraction"    >:: test_norm_builtin_sub;
    "Multiplication" >:: test_norm_builtin_mul;
    "Division"       >:: test_norm_builtin_div;
    "Modulus"        >:: test_norm_builtin_mod;
    "Exponentiation" >:: test_norm_builtin_exp;
    "Type Promotion" >:: test_norm_builtin_promote;
    "Concatenation"  >:: test_norm_builtin_concat;
  ]
