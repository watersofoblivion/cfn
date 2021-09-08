(* Builtins *)

open OUnit2

open CommonTest

(* Tests *)

let test_convert_builtin_add ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_add Mono.ty_int in
  let clos = Clos.builtin_add Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_sub ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_sub Mono.ty_int in
  let clos = Clos.builtin_sub Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_mul ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_mul Mono.ty_int in
  let clos = Clos.builtin_mul Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_div ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_div Mono.ty_int in
  let clos = Clos.builtin_div Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_mod ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_mod Mono.ty_int in
  let clos = Clos.builtin_mod Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_exp ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_exp Mono.ty_float in
  let clos = Clos.builtin_exp Clos.ty_float in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_promote ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_promote Mono.ty_int Mono.ty_long in
  let clos = Clos.builtin_promote Clos.ty_int Clos.ty_long in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_concat ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_concat Mono.ty_string in
  let clos = Clos.builtin_concat Clos.ty_string in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

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
