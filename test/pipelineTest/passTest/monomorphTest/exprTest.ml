(* Expressions *)

open OUnit2

open CommonTest

(* Tests *)

let test_convert_expr_builtin ctxt =
  let env = EnvTest.fresh () in
  let values = [1l; 2l] in
  let ir =
    let builtin = Ir.builtin_add Ir.ty_int in
    values
      |> List.map Ir.atom_int
      |> Ir.expr_builtin builtin
  in
  let mono =
    let builtin = Mono.builtin_add Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  Monomorph.convert_expr env ir (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_int ty;
    MonoTest.assert_expr_equal ~ctxt mono expr)

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.atom_bool |> Ir.expr_atom in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom in
  Monomorph.convert_expr env ir (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_expr_equal ~ctxt mono expr)

(* Test Suite *)

let suite =
  "Expressions" >::: [
    "Built-in Function Application" >:: test_convert_expr_builtin;
    "Atomic Values"                 >:: test_convert_expr_atom;
  ]
