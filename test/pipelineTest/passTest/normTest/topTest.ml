(* Top-Level Expressions *)

open OUnit2

open Common

open CommonTest

(* Tests *)

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

(* Test Suite *)

let suite =
  "Top-Level Expressions" >::: [
    "Let Bindings" >:: test_norm_top_let;
  ]
