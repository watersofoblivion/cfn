open OUnit2

open CommonTest

(* Assertions *)

let builtin_not_equal = TestUtils.not_equal "Built-in functions" Ir.pp_builtin

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.BuiltinAdd expected, Ir.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinSub expected, Ir.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinMul expected, Ir.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinDiv expected, Ir.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinMod expected, Ir.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinExp expected, Ir.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinPromote expected, Ir.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Ir.BuiltinConcat expected, Ir.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

let test_builtin_add ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_add ty in
  match expected with
    | Ir.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_sub ty in
  match expected with
    | Ir.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_mul ty in
  match expected with
    | Ir.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_div ty in
  match expected with
    | Ir.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_mod ty in
  match expected with
    | Ir.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_exp ty in
  match expected with
    | Ir.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Ir.ty_int in
  let sup = Ir.ty_long in
  let expected = Ir.builtin_promote sub sup in
  match expected with
    | Ir.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_concat ty in
  match expected with
    | Ir.BuiltinConcat actual ->
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
