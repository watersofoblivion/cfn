open OUnit2

open Common

open CommonTest

let test_norm_ty_bool ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.bool in
  let ir = Ir.Type.bool in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_int ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.int in
  let ir = Ir.Type.int in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_long ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.long in
  let ir = Ir.Type.long in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_float ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.float in
  let ir = Ir.Type.float in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_double ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.double in
  let ir = Ir.Type.double in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_rune ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.rune in
  let ir = Ir.Type.rune in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_ty_string ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Type.string in
  let ir = Ir.Type.string in
  IrTest.TypeTest.assert_ty_equal ~ctxt ir
    |> Norm.norm_ty env annot

let test_norm_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Ast.bool true in
  let ir =
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.bool ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_int ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Ast.int 42l in
  let ir =
    42l
      |> Ir.Ast.atom_int
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.int ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_long ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Ast.long 42L in
  let ir =
    42L
      |> Ir.Ast.atom_long
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.long ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_float ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Ast.float 4.2 in
  let ir =
    4.2
      |> Ir.Ast.atom_float
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.float ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_double ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Ast.double 4.2 in
  let ir =
    4.2
      |> Ir.Ast.atom_double
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.double ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let annot = Annot.Ast.rune value in
  let ir =
    value
      |> Ir.Ast.atom_rune
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.rune ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let annot = Annot.Ast.string value in
  let ir =
    value
      |> Ir.Ast.atom_string
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Norm.norm_expr env annot (fun ty block ->
    IrTest.TypeTest.assert_ty_equal ~ctxt Ir.Type.string ty;
    IrTest.AstTest.assert_block_equal ~ctxt ir block)

let test_norm_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Ir.Type.bool in
  let annot = Annot.Ast.ident value in
  let ir =
    value
      |> Ir.Ast.atom_ident
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
  in
  Env.bind value bound env (fun env ->
    Norm.norm_expr env annot (fun ty block ->
      IrTest.TypeTest.assert_ty_equal ~ctxt bound ty;
      IrTest.AstTest.assert_block_equal ~ctxt ir block))

let test_norm_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let annot = Annot.Ast.ident id in
  let exn = Norm.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Norm.norm_expr env annot (fun _ _ ->
      assert_failure "Expected exception"))

let test_norm_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let annot = Annot.Ast.patt_ground in
  let ir = Ir.Ast.patt_ground in
  Norm.norm_patt env annot Ir.Type.bool (fun _ patt ->
    IrTest.AstTest.assert_patt_equal ~ctxt ir patt)

let test_norm_patt_var ctxt =
  let env = EnvTest.fresh () in
  let annot =
    ()
      |> Sym.seq
      |> Sym.gen
      |> Annot.Ast.patt_var
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ir = Ir.Ast.patt_var id in
  let ty = Ir.Type.bool in
  Norm.norm_patt env annot ty (fun env patt ->
    EnvTest.assert_bound ~ctxt IrTest.TypeTest.assert_ty_equal id env ty;
    IrTest.AstTest.assert_patt_equal ~ctxt ir patt)

let test_norm_binding ctxt =
  let env = EnvTest.fresh () in
  let annot =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Annot.Ast.patt_var
    in
    let ty = Annot.Type.bool in
    true
      |> Annot.Ast.bool
      |> Annot.Ast.binding patt ty
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Ir.Type.bool in
  let ir =
    let patt = Ir.Ast.patt_var id in
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.binding patt ty
  in
  Norm.norm_binding env annot (fun env binding ->
    EnvTest.assert_bound ~ctxt IrTest.TypeTest.assert_ty_equal id env ty;
    IrTest.AstTest.assert_binding_equal ~ctxt ir binding)

let test_norm_top_let ctxt =
  let env = EnvTest.fresh () in
  let annot =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Annot.Ast.patt_var
    in
    let ty = Annot.Type.bool in
    true
      |> Annot.Ast.bool
      |> Annot.Ast.binding patt ty
      |> Annot.Ast.top_let
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Ir.Type.bool in
  let ir =
    let patt = Ir.Ast.patt_var id in
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.binding patt ty
      |> Ir.Ast.top_let
  in
  Norm.norm_top env annot (fun env top ->
    EnvTest.assert_bound ~ctxt IrTest.TypeTest.assert_ty_equal id env ty;
    IrTest.AstTest.assert_top_equal ~ctxt ir top)

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
