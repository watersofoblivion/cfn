(* Abstract Syntax *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let assert_desug_top top ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env sym ->
    let syntax =
      let patt =
        let loc = LocTest.gen () in
        ()
          |> Sym.seq
          |> Sym.gen
          |> Syntax.patt_var loc
      in
      let ty =
        let loc = LocTest.gen () in
        Syntax.ty_constr loc sym
      in
      let value =
        let loc = LocTest.gen () in
        Syntax.expr_bool loc true
      in
      let binding =
        let loc = LocTest.gen () in
        Syntax.value_binding loc patt (Some ty) value
      in
      let loc = LocTest.gen () in
      top loc binding
    in
    let ty = Annot.ty_bool in
    let id = () |> Sym.seq |> Sym.gen in
    let annot =
      let patt = Annot.patt_var id in
      true
        |> Annot.expr_bool
        |> Annot.binding patt ty
        |> Annot.top_let
    in
    Desug.desug_top env syntax (fun env top ->
      EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty;
      AnnotTest.assert_top_equal ~ctxt annot top))

let test_desug_top_let = assert_desug_top Syntax.top_let
let test_desug_top_val = assert_desug_top Syntax.top_val

let suite =
  "Top-Level Expressions" >::: [
    "Let Bindings" >:: test_desug_top_let;
    "Val Bindings" >:: test_desug_top_val;
  ]
