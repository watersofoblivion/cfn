open OUnit2

open Common

open CommonTest

let test_norm_ty_bool ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_bool in
  let ir = Ir.ty_bool in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_int ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_int in
  let ir = Ir.ty_int in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_long ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_long in
  let ir = Ir.ty_long in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_float ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_float in
  let ir = Ir.ty_float in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_double ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_double in
  let ir = Ir.ty_double in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_rune ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_rune in
  let ir = Ir.ty_rune in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_string ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.ty_string in
  let ir = Ir.ty_string in
  IrTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_bool true in
  let ir =
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_bool ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_int ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_int 42l in
  let ir =
    42l
      |> Ir.atom_int
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_int ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_long ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_long 42L in
  let ir =
    42L
      |> Ir.atom_long
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_long ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_float ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_float 4.2 in
  let ir =
    4.2
      |> Ir.atom_float
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_float ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_double ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.expr_double 4.2 in
  let ir =
    4.2
      |> Ir.atom_double
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_double ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let annot = Annot.expr_rune value in
  let ir =
    value
      |> Ir.atom_rune
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_rune ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let annot = Annot.expr_string value in
  let ir =
    value
      |> Ir.atom_string
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.assert_ty_equal ~ctxt Ir.ty_string ty;
    IrTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Ir.ty_bool in
  let annot = Annot.expr_ident value in
  let ir =
    value
      |> Ir.atom_ident
      |> Ir.expr_atom
      |> Ir.block_expr
  in
  Env.bind value bound env (fun env ->
    Norm.norm_expr env annot (fun ty block ->
      IrTest.assert_ty_equal ~ctxt bound ty;
      IrTest.assert_block_equal ~ctxt ir block))

let test_norm_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let annot = Annot.expr_ident id in
  let exn = Norm.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Norm.norm_expr env annot (fun _ _ ->
      assert_failure "Expected exception"))

let test_norm_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.patt_ground in
  let ir = Ir.patt_ground in
  Norm.norm_patt env annot Ir.ty_bool (fun _ patt ->
    IrTest.assert_patt_equal ~ctxt ir patt)

let test_norm_patt_var ctxt =
  let env = EnvTest.fresh () in
  let annot =
    ()
      |> Sym.seq
      |> Sym.gen
      |> Annot.patt_var
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ir = Ir.patt_var id in
  let ty = Ir.ty_bool in
  Norm.norm_patt env annot ty (fun env patt ->
    EnvTest.assert_bound ~ctxt IrTest.assert_ty_equal id env ty;
    IrTest.assert_patt_equal ~ctxt ir patt)

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

let test_norm_top_let ctxt =
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
      |> Annot.top_let
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Ir.ty_bool in
  let ir =
    let patt = Ir.patt_var id in
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding patt ty
      |> Ir.top_let
  in
  Norm.norm_top env annot (fun env top ->
    EnvTest.assert_bound ~ctxt IrTest.assert_ty_equal id env ty;
    IrTest.assert_top_equal ~ctxt ir top)

let suite =
  "A-Normalization" >::: [
    "Types" >::: [
      "Boolean" >:: test_norm_ty_bool;
      "Integer" >:: test_norm_ty_int;
      "Long"    >:: test_norm_ty_long;
      "Float"   >:: test_norm_ty_float;
      "Double"  >:: test_norm_ty_double;
      "Rune"    >:: test_norm_ty_rune;
      "String"  >:: test_norm_ty_string;
    ];
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
    "Patterns" >::: [
      "Ground"      >:: test_norm_patt_ground;
      "Identifiers" >:: test_norm_patt_var;
    ];
    "Bindings" >:: test_norm_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_norm_top_let;
    ];
  ]
