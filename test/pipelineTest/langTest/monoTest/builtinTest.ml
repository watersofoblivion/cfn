open OUnit2

open CommonTest

(* Assertions *)

let builtin_not_equal = TestUtils.not_equal "Built-in functions" Mono.pp_builtin

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.BuiltinAdd expected, Mono.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinSub expected, Mono.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinMul expected, Mono.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinDiv expected, Mono.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinMod expected, Mono.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinExp expected, Mono.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinPromote expected, Mono.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Mono.BuiltinConcat expected, Mono.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

let test_builtin_add ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_add ty in
  match expected with
    | Mono.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_sub ty in
  match expected with
    | Mono.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_mul ty in
  match expected with
    | Mono.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_div ty in
  match expected with
    | Mono.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_mod ty in
  match expected with
    | Mono.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_exp ty in
  match expected with
    | Mono.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Mono.ty_int in
  let sup = Mono.ty_long in
  let expected = Mono.builtin_promote sub sup in
  match expected with
    | Mono.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_concat ty in
  match expected with
    | Mono.BuiltinConcat actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

(* Test Suite *)

let suite =
  "Built-In Functions" >::: [
    "Constructors" >::: [
      "Addition"       >:: test_builtin_add;
      "Subtraction"    >:: test_builtin_sub;
      "Multiplication" >:: test_builtin_mul;
      "Division"       >:: test_builtin_div;
      "Modulus"        >:: test_builtin_mod;
      "Exponentiation" >:: test_builtin_exp;
      "Type Promotion" >:: test_builtin_promote;
      "Concatenation"  >:: test_builtin_concat;
    ];
  ]
