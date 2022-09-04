(* Expressions *)

open OUnit2

open CommonTest

(* Tests *)

let test_convert_expr_builtin_fixed_arity ctxt =
  let env = EnvTest.fresh () in
  let values = [1l; 2l] in
  let mono =
    let builtin = Mono.builtin_struct_eq Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  let clos =
    let builtin = Clos.builtin_struct_eq Clos.ty_int in
    values
      |> List.map Clos.atom_int
      |> Clos.expr_builtin builtin
  in
  Conv.convert_expr env mono (fun ty expr ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_expr_equal ~ctxt clos expr)

let test_convert_expr_builtin_var_arity ctxt =
  let env = EnvTest.fresh () in
  let values = ["foo"; "bar"; "baz"] in
  let mono =
    let builtin = Mono.builtin_concat Mono.ty_string in
    values
      |> List.map Mono.atom_string
      |> Mono.expr_builtin builtin
  in
  let clos =
    let builtin = Clos.builtin_concat Clos.ty_string in
    values
      |> List.map Clos.atom_string
      |> Clos.expr_builtin builtin
  in
  Conv.convert_expr env mono (fun ty expr ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_string ty;
    ClosTest.assert_expr_equal ~ctxt clos expr)

let test_convert_expr_builtin_invalid_arity _ =
  let env = EnvTest.fresh () in
  let values = [1l; 2l; 3l] in
  let mono =
    let builtin = Mono.builtin_struct_eq Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  Conv.convert_expr env mono
    |> CheckTest.assert_raises_invalid_arity 2 3

let test_convert_expr_builtin_mismatched_types _ =
  let env = EnvTest.fresh () in
  let values = [1L; 2L] in
  let mono =
    let builtin = Mono.builtin_struct_eq Mono.ty_int in
    values
      |> List.map Mono.atom_long
      |> Mono.expr_builtin builtin
  in
  Conv.convert_expr env mono
    |> CheckTest.assert_raises_mismatched_types Clos.ty_long Clos.ty_int

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
    "Built-in Function Application" >::: [
      "Fixed Arity"      >:: test_convert_expr_builtin_fixed_arity;
      "Variable Arity"   >:: test_convert_expr_builtin_var_arity;
      "Invalid Arity"    >:: test_convert_expr_builtin_invalid_arity;
      "Mismatched Types" >:: test_convert_expr_builtin_mismatched_types;
    ];
    "Atomic Values"                 >:: test_convert_expr_atom;
  ]
