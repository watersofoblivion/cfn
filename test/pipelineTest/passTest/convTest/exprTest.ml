(* Expressions *)

open OUnit2

open CommonTest

(* Tests *)

let test_convert_expr_builtin ctxt =
  let env = EnvTest.fresh () in
  let values = [1l; 2l] in
  let mono =
    let builtin = Mono.builtin_add Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  let clos =
    let builtin = Clos.builtin_add Clos.ty_int in
    values
      |> List.map Clos.atom_int
      |> Clos.expr_builtin builtin
  in
  Conv.convert_expr env mono (fun ty expr ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_int ty;
    ClosTest.assert_expr_equal ~ctxt clos expr)

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom in
  let clos = true |> Clos.atom_bool |> Clos.expr_atom in
  Conv.convert_expr env mono (fun ty expr ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_expr_equal ~ctxt clos expr)

(* Test Suite *)

let suite =
  "Expressions" >::: [
    "Built-in Function Application" >:: test_convert_expr_builtin;
    "Atomic Values"                 >:: test_convert_expr_atom;
  ]
