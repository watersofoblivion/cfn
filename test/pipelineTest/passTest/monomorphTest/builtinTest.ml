(* Builtins *)

open OUnit2

open CommonTest

(* Tests *)

let test_convert_builtin_add ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_add Ir.ty_int in
  let mono = Mono.builtin_add Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_sub ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_sub Ir.ty_int in
  let mono = Mono.builtin_sub Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_mul ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_mul Ir.ty_int in
  let mono = Mono.builtin_mul Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_div ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_div Ir.ty_int in
  let mono = Mono.builtin_div Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_mod ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_mod Ir.ty_int in
  let mono = Mono.builtin_mod Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_exp ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_exp Ir.ty_float in
  let mono = Mono.builtin_exp Mono.ty_float in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_promote ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_promote Ir.ty_int Ir.ty_long in
  let mono = Mono.builtin_promote Mono.ty_int Mono.ty_long in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_concat ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_concat Ir.ty_string in
  let mono = Mono.builtin_concat Mono.ty_string in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

(* Test Suite *)

let suite =
  "Built-in Functions" >::: [
    "Addition"       >:: test_convert_builtin_add;
    "Subtraction"    >:: test_convert_builtin_sub;
    "Multiplication" >:: test_convert_builtin_mul;
    "Division"       >:: test_convert_builtin_div;
    "Modulus"        >:: test_convert_builtin_mod;
    "Exponentiation" >:: test_convert_builtin_exp;
    "Type Promotion" >:: test_convert_builtin_promote;
    "Concatenation"  >:: test_convert_builtin_concat;
  ]
