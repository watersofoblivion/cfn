(* Patterns *)

open OUnit2

open Common

open CommonTest

(* Tests *)

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

(* Test Suite *)

let suite =
  "Patterns" >::: [
    "Ground"      >:: test_norm_patt_ground;
    "Identifiers" >:: test_norm_patt_var;
  ]
