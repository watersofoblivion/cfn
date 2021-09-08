(* Patterns *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_desug_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    LocTest.gen ()
      |> Syntax.patt_ground
  in
  let annot = Annot.patt_ground in
  Desug.desug_patt env syntax Annot.ty_bool (fun _ patt ->
    AnnotTest.assert_patt_equal ~ctxt annot patt)

let test_desug_patt_var ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    ()
      |> Sym.seq
      |> Sym.gen
      |> Syntax.patt_var loc
  in
  let id = () |> Sym.seq |> Sym.gen in
  let annot = Annot.patt_var id in
  let ty = Annot.ty_bool in
  Desug.desug_patt env syntax ty (fun env patt ->
    EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty;
    AnnotTest.assert_patt_equal ~ctxt annot patt)

(* Test Suite *)

let suite =
  "Patterns" >::: [
    "Ground"      >:: test_desug_patt_ground;
    "Identifiers" >:: test_desug_patt_var;
  ]
