(* Expresions *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_norm_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_bool true in
  let ir =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_bool ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_int ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_int 42l in
  let ir =
    42l
      |> Ir.atom_int
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_int ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_long ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_long 42L in
  let ir =
    42L
      |> Ir.atom_long
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_long ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_float ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_float 4.2 in
  let ir =
    4.2
      |> Ir.atom_float
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_float ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_double ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_double 4.2 in
  let ir =
    4.2
      |> Ir.atom_double
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_double ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let annot = Annot.expr_rune value in
  let ir =
    value
      |> Ir.atom_rune
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_rune ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_string ctxt =
  let env = EnvTest.fresh () in
  let value = "foo bar" in
  let annot = Annot.expr_string value in
  let ir =
    value
      |> Ir.atom_string
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Norm.norm_expr env annot (fun ty term ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_string ty;
    IrTest.assert_term_equal ~ctxt ir term)

let test_norm_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Ir.ty_bool in
  let annot = Annot.expr_ident value in
  let ir =
    value
      |> Ir.atom_ident
      |> Ir.expr_atom
      |> Ir.term_expr
  in
  Env.bind value bound env (fun env ->
    Norm.norm_expr env annot (fun ty term ->
      IrTest.assert_ty_equal ~ctxt bound ty;
      IrTest.assert_term_equal ~ctxt ir term))

let test_norm_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let annot = Annot.expr_ident id in
  Norm.norm_expr env annot
    |> CheckTest.assert_raises_unbound_identifier id

(* Bindings *)

let test_norm_binding ctxt =
  let env = EnvTest.fresh () in
  let annot =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Annot.patt_var
    in
    let ty = Annot.ty_bool in
    true
      |> Annot.expr_bool
      |> Annot.binding patt ty
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Ir.ty_bool in
  let ir =
    let patt = Ir.patt_var id in
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding patt ty
  in
  Norm.norm_binding env annot (fun env binding ->
    EnvTest.assert_bound ~ctxt IrTest.assert_ty_equal id env ty;
    IrTest.assert_binding_equal ~ctxt ir binding)

(* Test Suite *)

let suite =
  "Expressions" >::: [
    "Expressions" >::: [
      "Booleans" >:: test_norm_expr_bool;
      "Integers" >:: test_norm_expr_int;
      "Longs"    >:: test_norm_expr_long;
      "Floats"   >:: test_norm_expr_float;
      "Doubles"  >:: test_norm_expr_double;
      "Runes"    >:: test_norm_expr_rune;
      "Strings"  >:: test_norm_expr_string;
      "Identifiers" >::: [
        "Bound"   >:: test_norm_expr_ident;
        "Unbound" >:: test_norm_expr_ident_unbound;
      ];
    ];
    "Bindings" >:: test_norm_binding;
  ]
