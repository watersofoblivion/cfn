open OUnit2

open CommonTest

(* Assertions *)

let builtin_not_equal = TestUtils.not_equal "Built-in functions" Annot.pp_builtin

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.BuiltinAdd expected, Annot.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinSub expected, Annot.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinMul expected, Annot.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinDiv expected, Annot.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinMod expected, Annot.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinExp expected, Annot.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinPromote expected, Annot.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Annot.BuiltinConcat expected, Annot.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

let test_builtin_add ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_add ty in
  match expected with
    | Annot.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_sub ty in
  match expected with
    | Annot.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_mul ty in
  match expected with
    | Annot.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_div ty in
  match expected with
    | Annot.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_mod ty in
  match expected with
    | Annot.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_exp ty in
  match expected with
    | Annot.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Annot.ty_int in
  let sup = Annot.ty_long in
  let expected = Annot.builtin_promote sub sup in
  match expected with
    | Annot.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Annot.ty_string in
  let expected = Annot.builtin_concat ty in
  match expected with
    | Annot.BuiltinConcat actual ->
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
