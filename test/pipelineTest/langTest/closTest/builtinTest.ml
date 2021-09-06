open OUnit2

open CommonTest

(* Assertions *)

let builtin_not_equal = TestUtils.not_equal "Built-in functions" Clos.pp_builtin

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.BuiltinAdd expected, Clos.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinSub expected, Clos.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinMul expected, Clos.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinDiv expected, Clos.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinMod expected, Clos.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinExp expected, Clos.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinPromote expected, Clos.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Clos.BuiltinConcat expected, Clos.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

let test_builtin_add ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_add ty in
  match expected with
    | Clos.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_sub ty in
  match expected with
    | Clos.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_mul ty in
  match expected with
    | Clos.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_div ty in
  match expected with
    | Clos.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_mod ty in
  match expected with
    | Clos.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_exp ty in
  match expected with
    | Clos.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Clos.ty_int in
  let sup = Clos.ty_long in
  let expected = Clos.builtin_promote sub sup in
  match expected with
    | Clos.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_concat ty in
  match expected with
    | Clos.BuiltinConcat actual ->
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
