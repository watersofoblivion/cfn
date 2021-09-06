open OUnit2

open Common

open CommonTest

(* Assertions *)

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal

(* Test Suite *)

let test_check_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let b = Annot.expr_bool true in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_bool
    |> Annot.check_expr env b

let test_check_expr_int ctxt =
  let env = EnvTest.fresh () in
  let i = Annot.expr_int 42l in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_int
    |> Annot.check_expr env i

let test_check_expr_long ctxt =
  let env = EnvTest.fresh () in
  let l = Annot.expr_long 42L in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_long
    |> Annot.check_expr env l

let test_check_expr_float ctxt =
  let env = EnvTest.fresh () in
  let f = Annot.expr_float 4.2 in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_float
    |> Annot.check_expr env f

let test_check_expr_double ctxt =
  let env = EnvTest.fresh () in
  let d = Annot.expr_double 4.2 in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_double
    |> Annot.check_expr env d

let test_check_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let r =
    'a'
      |> Uchar.of_char
      |> Annot.expr_rune
  in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_rune
    |> Annot.check_expr env r

let test_check_expr_string ctxt =
  let env = EnvTest.fresh () in
  let s = Annot.expr_string "foo bar" in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_string
    |> Annot.check_expr env s

let test_check_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Annot.expr_ident id in
  let ty = Annot.ty_bool in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Annot.check_expr env ident)

let test_check_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Annot.expr_ident id in
  let exn = Annot.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Annot.check_expr env ident (fun _ ->
      assert_failure "Expected exception"))

let test_check_patt_ground _ =
  let env = EnvTest.fresh () in
  let patt = Annot.patt_ground in
  let ty = Annot.ty_bool in
  Annot.check_patt env patt ty (fun _ -> ())

let test_check_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Annot.patt_var id in
  let ty = Annot.ty_bool in
  Annot.check_patt env patt ty (fun env ->
    assert_ty_bound ~ctxt id env ty)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Annot.patt_var id in
  let ty = Annot.ty_bool in
  let value = Annot.expr_bool true in
  let binding = Annot.binding patt ty value in
  Annot.check_binding env binding (fun env ->
    assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Annot.patt_var id in
  let inferred = Annot.ty_bool in
  let annotated = Annot.ty_int in
  let value = Annot.expr_bool true in
  let binding = Annot.binding patt annotated value in
  let exn = Annot.MismatchedTypes (value, inferred, annotated) in
  assert_raises exn (fun _ ->
    Annot.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Annot.ty_bool in
  let top =
    let patt = Annot.patt_var id in
    true
      |> Annot.expr_bool
      |> Annot.binding patt ty
      |> Annot.top_let
  in
  Annot.check_top env top (fun env ->
    assert_ty_bound ~ctxt id env ty)

let suite =
  "Type Checking" >::: [
    "Expressions" >::: [
      "Booleans"    >:: test_check_expr_bool;
      "Integers"    >:: test_check_expr_int;
      "Longs"       >:: test_check_expr_long;
      "Float"       >:: test_check_expr_float;
      "Double"      >:: test_check_expr_double;
      "Rune"        >:: test_check_expr_rune;
      "String"      >:: test_check_expr_string;
      "Identifiers" >::: [
        "Bound"   >:: test_check_expr_ident;
        "Unbound" >:: test_check_expr_ident_unbound
      ];
    ];
    "Patterns" >::: [
      "Ground"   >:: test_check_patt_ground;
      "Variable" >:: test_check_patt_var;
    ];
    "Bindings" >::: [
      "Valid"            >:: test_check_binding;
      "Mismatched Types" >:: test_check_binding_mismatched_types;
    ];
    "Top-Levels" >::: [
      "Let Binding" >:: test_check_top_let;
    ]
  ]
