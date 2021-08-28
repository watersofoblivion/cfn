open OUnit2

open Common
open Annot

open CommonTest

(* Assertions *)

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal

(* Test Suite *)

let test_check_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let b = Ast.bool true in
  TypeTest.assert_ty_equal ~ctxt Type.bool
    |> Check.check_expr env b

let test_check_expr_int ctxt =
  let env = EnvTest.fresh () in
  let i = Ast.int 42l in
  TypeTest.assert_ty_equal ~ctxt Type.int
    |> Check.check_expr env i

let test_check_expr_long ctxt =
  let env = EnvTest.fresh () in
  let l = Ast.long 42L in
  TypeTest.assert_ty_equal ~ctxt Type.long
    |> Check.check_expr env l

let test_check_expr_float ctxt =
  let env = EnvTest.fresh () in
  let f = Ast.float 4.2 in
  TypeTest.assert_ty_equal ~ctxt Type.float
    |> Check.check_expr env f

let test_check_expr_double ctxt =
  let env = EnvTest.fresh () in
  let d = Ast.double 4.2 in
  TypeTest.assert_ty_equal ~ctxt Type.double
    |> Check.check_expr env d

let test_check_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let r =
    'a'
      |> Uchar.of_char
      |> Ast.rune
  in
  TypeTest.assert_ty_equal ~ctxt Type.rune
    |> Check.check_expr env r

let test_check_expr_string ctxt =
  let env = EnvTest.fresh () in
  let s =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
      |> Ast.string
  in
  TypeTest.assert_ty_equal ~ctxt Type.string
    |> Check.check_expr env s

let test_check_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Ast.ident id in
  let ty = Type.bool in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Check.check_expr env ident)

let test_check_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Ast.ident id in
  let exn = Check.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Check.check_expr env ident (fun _ ->
      assert_failure "Expected exception"))

let test_check_patt_ground _ =
  let env = EnvTest.fresh () in
  let patt = Ast.patt_ground in
  let ty = Type.bool in
  Check.check_patt env patt ty (fun _ -> ())

let test_check_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Ast.patt_var id in
  let ty = Type.bool in
  Check.check_patt env patt ty (fun env ->
    assert_ty_bound ~ctxt id env ty)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Ast.patt_var id in
  let ty = Type.bool in
  let value = Ast.bool true in
  let binding = Ast.binding patt ty value in
  Check.check_binding env binding (fun env ->
    assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Ast.patt_var id in
  let inferred = Type.bool in
  let annotated = Type.int in
  let value = Ast.bool true in
  let binding = Ast.binding patt annotated value in
  let exn = Check.MismatchedTypes (inferred, annotated) in
  assert_raises exn (fun _ ->
    Check.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Type.bool in
  let top =
    let patt = Ast.patt_var id in
    true
      |> Ast.bool
      |> Ast.binding patt ty
      |> Ast.top_let
  in
  Check.check_top env top (fun env ->
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
