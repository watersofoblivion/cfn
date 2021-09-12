(* Expressions *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let fresh_expr_builtin ?fn:(fn = BuiltinTest.fresh_builtin_add ()) ?args:(args = [AtomTest.fresh_atom_int (); AtomTest.fresh_atom_int ()]) _ =
  Ir.expr_builtin fn args

let fresh_expr_atom ?atom:(atom = AtomTest.fresh_atom_bool ()) _ =
  Ir.expr_atom atom

(* Assertions *)

let expr_not_equal = TestUtils.not_equal "Expressions" Ir.pp_expr

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.ExprBuiltin expected, Ir.ExprBuiltin actual ->
    BuiltinTest.assert_builtin_equal ~ctxt expected.fn actual.fn;
    List.iter2 (AtomTest.assert_atom_equal ~ctxt) expected.args actual.args
  | Ir.ExprAtom expected, Ir.ExprAtom actual ->
    AtomTest.assert_atom_equal ~ctxt expected.atom actual.atom
  | expected, actual -> expr_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_expr_builtin ctxt =
  let fn = BuiltinTest.fresh_builtin_add () in
  let args = [AtomTest.fresh_atom_int (); AtomTest.fresh_atom_long ()] in
  let expected = Ir.expr_builtin fn args in
  match expected with
    | Ir.ExprBuiltin actual ->
      BuiltinTest.assert_builtin_equal ~ctxt fn actual.fn;
      List.iter2 (AtomTest.assert_atom_equal ~ctxt) args actual.args
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_atom ctxt =
  let atom = AtomTest.fresh_atom_bool () in
  let expected = Ir.expr_atom atom in
  match expected with
    | Ir.ExprAtom actual ->
      AtomTest.assert_atom_equal ~ctxt atom actual.atom
    | actual -> expr_not_equal ~ctxt expected actual

let test_constructors =
  "Constructors" >::: [
    "Built-in Function Application" >:: test_expr_builtin;
    "Atoms"                         >:: test_expr_atom;
  ]

(* Pretty Printing *)

let assert_pp_expr = PrettyTest.assert_pp Ir.pp_expr

let test_pp_expr_builtin ctxt =
  let fn = BuiltinTest.fresh_builtin_struct_eq () in
  let args = [
    AtomTest.fresh_atom_bool ();
    AtomTest.fresh_atom_int ();
  ] in
  let pp_sep fmt _ = fprintf fmt " " in
  fresh_expr_builtin ~fn ~args ()
    |> assert_pp_expr ~ctxt [
         fprintf str_formatter "%a %a" Ir.pp_builtin fn (pp_print_list ~pp_sep Ir.pp_atom) args
           |> flush_str_formatter;
       ]

let test_pp_expr_atom ctxt =
  let atom = AtomTest.fresh_atom_bool ~value:true () in
  fresh_expr_atom ~atom ()
    |> assert_pp_expr ~ctxt ["true"]

let test_pp =
  "Pretty Printing" >::: [
    "Built-in Function Application" >:: test_pp_expr_builtin;
    "Atoms"                         >:: test_pp_expr_atom;
  ]

(* Type Checking *)

let test_check_expr_builtin_fixed ctxt =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_struct_eq ~ty:Ir.ty_int () in
  let args = [AtomTest.fresh_atom_int (); AtomTest.fresh_atom_int ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_expr env expr

let test_check_expr_builtin_var ctxt =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_concat ~ty:Ir.ty_string () in
  let args = [AtomTest.fresh_atom_string (); AtomTest.fresh_atom_string (); AtomTest.fresh_atom_string ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_string
    |> Ir.check_expr env expr

let test_check_expr_builtin_invalid_arity _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_add ~ty:Ir.ty_int () in
  let expr = fresh_expr_builtin ~fn ~args:[] () in
  let exn = Ir.InvalidArity (2, 0) in
  assert_raises exn (fun _ ->
    Ir.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_builtin_fixed_mismatched_types _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_add ~ty:Ir.ty_int () in
  let args = [AtomTest.fresh_atom_int (); AtomTest.fresh_atom_bool ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  let exn = Ir.MismatchedTypes (Ir.ty_bool, Ir.ty_int) in
  assert_raises exn (fun _ ->
    Ir.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_builtin_var_mismatched_types _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_concat ~ty:Ir.ty_string () in
  let args = [AtomTest.fresh_atom_string (); AtomTest.fresh_atom_int (); AtomTest.fresh_atom_string ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  let exn = Ir.MismatchedTypes (Ir.ty_int, Ir.ty_string) in
  assert_raises exn (fun _ ->
    Ir.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let expr =
    let atom = AtomTest.fresh_atom_bool () in
    fresh_expr_atom ~atom ()
  in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_expr env expr

let test_check =
  "Type Checking" >::: [
    "Built-in Function Application" >::: [
      "Valid"                     >::: [
        "Fixed Arity"    >:: test_check_expr_builtin_fixed;
        "Variable Arity" >:: test_check_expr_builtin_var;
      ];
      "Invalid Arity"             >:: test_check_expr_builtin_invalid_arity;
      "Mismatched Argument Types" >::: [
        "Fixed Arity"    >:: test_check_expr_builtin_fixed_mismatched_types;
        "Variable Arity" >:: test_check_expr_builtin_var_mismatched_types;
      ];
    ];
    "Atoms" >:: test_check_expr_atom;
  ]

(* Test Suite *)

let suite =
  "Expressions" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
