open OUnit2

open Common

open CommonTest

(* Assertions *)

let assert_ty_bound = EnvTest.assert_bound TypeTest.assert_ty_equal

(* Test Suite *)

let test_check_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let b = Ir.atom_bool true in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_atom env b

let test_check_atom_int ctxt =
  let env = EnvTest.fresh () in
  let i = Ir.atom_int 42l in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_int
    |> Ir.check_atom env i

let test_check_atom_long ctxt =
  let env = EnvTest.fresh () in
  let l = Ir.atom_long 42L in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_long
    |> Ir.check_atom env l

let test_check_atom_float ctxt =
  let env = EnvTest.fresh () in
  let f = Ir.atom_float 4.2 in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_float
    |> Ir.check_atom env f

let test_check_atom_double ctxt =
  let env = EnvTest.fresh () in
  let d = Ir.atom_double 4.2 in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_double
    |> Ir.check_atom env d

let test_check_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let r =
    'a'
      |> Uchar.of_char
      |> Ir.atom_rune
  in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_rune
    |> Ir.check_atom env r

let test_check_atom_string ctxt =
  let env = EnvTest.fresh () in
  let s =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
      |> Ir.atom_string
  in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_string
    |> Ir.check_atom env s

let test_check_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Ir.atom_ident id in
  let ty = Ir.ty_bool in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Ir.check_atom env ident)

let test_check_atom_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Ir.atom_ident id in
  let exn = Ir.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Ir.check_atom env ident (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let expr =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
  in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_expr env expr

let test_check_block_expr ctxt =
  let env = EnvTest.fresh () in
  let block =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_block env block

let test_check_patt_ground _ =
  let env = EnvTest.fresh () in
  let patt = Ir.patt_ground in
  let ty = Ir.ty_bool in
  Ir.check_patt env patt ty (fun _ -> ())

let test_check_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Ir.patt_var id in
  let ty = Ir.ty_bool in
  Ir.check_patt env patt ty (fun env ->
    assert_ty_bound ~ctxt id env ty)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Ir.patt_var id in
  let ty = Ir.ty_bool in
  let value =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
  in
  let binding = Ir.binding patt ty value in
  Ir.check_binding env binding (fun env ->
    assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Ir.patt_var id in
  let inferred = Ir.ty_bool in
  let annotated = Ir.ty_int in
  let value =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
  in
  let binding = Ir.binding patt annotated value in
  let exn = Ir.MismatchedTypes (inferred, annotated) in
  assert_raises exn (fun _ ->
    Ir.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Ir.ty_bool in
  let top =
    let patt = Ir.patt_var id in
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding patt ty
      |> Ir.top_let
  in
  Ir.check_top env top (fun env ->
    assert_ty_bound ~ctxt id env ty)

let suite =
  "Type Checking" >::: [
    "Atoms" >::: [
      "Booleans"    >:: test_check_atom_bool;
      "Integers"    >:: test_check_atom_int;
      "Longs"       >:: test_check_atom_long;
      "Float"       >:: test_check_atom_float;
      "Double"      >:: test_check_atom_double;
      "Rune"        >:: test_check_atom_rune;
      "String"      >:: test_check_atom_string;
      "Identifiers" >::: [
        "Bound"   >:: test_check_atom_ident;
        "Unbound" >:: test_check_atom_ident_unbound
      ];
    ];
    "Expressions" >::: [
      "Atoms" >:: test_check_expr_atom;
    ];
    "Blocks" >::: [
      "Expressions" >:: test_check_block_expr;
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
