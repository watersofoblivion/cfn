(* Expressions *)

open OUnit2

open CommonTest

(* Tests *)

let test_convert_expr_builtin_fixed_arity ctxt =
  let env = EnvTest.fresh () in
  let values = [1l; 2l] in
  let mono =
    let builtin = Ir.builtin_struct_eq Ir.ty_int in
    values
      |> List.map Ir.atom_int
      |> Ir.expr_builtin builtin
  in
  let clos =
    let builtin = Mono.builtin_struct_eq Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  Monomorph.convert_expr env mono (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_expr_equal ~ctxt clos expr)

let test_convert_expr_builtin_var_arity ctxt =
  let env = EnvTest.fresh () in
  let values = ["foo"; "bar"; "baz"] in
  let mono =
    let builtin = Ir.builtin_concat Ir.ty_string in
    values
      |> List.map Ir.atom_string
      |> Ir.expr_builtin builtin
  in
  let clos =
    let builtin = Mono.builtin_concat Mono.ty_string in
    values
      |> List.map Mono.atom_string
      |> Mono.expr_builtin builtin
  in
  Monomorph.convert_expr env mono (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_string ty;
    MonoTest.assert_expr_equal ~ctxt clos expr)

let test_convert_expr_builtin_invalid_arity _ =
  let env = EnvTest.fresh () in
  let values = [1l; 2l; 3l] in
  let mono =
    let builtin = Ir.builtin_struct_eq Ir.ty_int in
    values
      |> List.map Ir.atom_int
      |> Ir.expr_builtin builtin
  in
  let exn = Monomorph.InvalidArity (2, 3) in
  assert_raises exn (fun _ ->
    Monomorph.convert_expr env mono (fun _ _ ->
      assert_failure "Expected exception"))

let test_convert_expr_builtin_mismatched_types _ =
  let env = EnvTest.fresh () in
  let values = [1L; 2L] in
  let mono =
    let builtin = Ir.builtin_struct_eq Ir.ty_int in
    values
      |> List.map Ir.atom_long
      |> Ir.expr_builtin builtin
  in
  let exn = Monomorph.MismatchedTypes (Mono.ty_long, Mono.ty_int) in
  assert_raises exn (fun _ ->
    Monomorph.convert_expr env mono (fun _ _ ->
      assert_failure "Expected exception"))

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Ir.atom_bool |> Ir.expr_atom in
  let clos = true |> Mono.atom_bool |> Mono.expr_atom in
  Monomorph.convert_expr env mono (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_expr_equal ~ctxt clos expr)

(* Test Suite *)

let suite =
  "Expressions" >::: [
    "Built-in Function Application" >::: [
      "Fixed Arity"      >:: test_convert_expr_builtin_fixed_arity;
      "Variable Arity"   >:: test_convert_expr_builtin_var_arity;
      "Invalid Arity"    >:: test_convert_expr_builtin_invalid_arity;
      "Mismatched Types" >:: test_convert_expr_builtin_mismatched_types;
    ];
    "Atomic Values"                 >:: test_convert_expr_atom;
  ]
