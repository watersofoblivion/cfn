(* Source Files *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_desug_file ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env bool_constr ->
    Env.constr_of Prim.id_int env (fun env int_constr ->
      let syntax =
        let seq = Sym.seq () in
        let top =
          let patt =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.patt_var loc
          in
          let ty =
            let loc = LocTest.gen () in
            Syntax.ty_constr loc bool_constr
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
          Syntax.top_let loc binding
        in
        let top' =
          let patt =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.patt_var loc
          in
          let ty =
            let loc = LocTest.gen () in
            Syntax.ty_constr loc int_constr
          in
          let value =
            let loc = LocTest.gen () in
            Syntax.expr_int loc "42"
          in
          let binding =
            let loc = LocTest.gen () in
            Syntax.value_binding loc patt (Some ty) value
          in
          let loc = LocTest.gen () in
          Syntax.top_val loc binding
        in
        let pkg =
          let name =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.name loc
          in
          let loc = LocTest.gen () in
          Syntax.pkg loc name
        in
        Syntax.file pkg [] [top; top']
      in
      let seq = Sym.seq () in
      let id = Sym.gen seq in
      let ty = Annot.ty_bool in
      let id' = Sym.gen seq in
      let ty' = Annot.ty_int in
      let annot =
        let top =
          let patt = Annot.patt_var id in
          true
            |> Annot.expr_bool
            |> Annot.binding patt ty
            |> Annot.top_let
        in
        let top' =
          let patt = Annot.patt_var id' in
          42l
            |> Annot.expr_int
            |> Annot.binding patt ty'
            |> Annot.top_let
        in
        [top; top']
      in
      Desug.desug_file env syntax (fun env tops ->
        List.iter (fun (id, ty) ->
          EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty
        ) [(id, ty); (id', ty')];
        List.iter2 (AnnotTest.assert_top_equal ~ctxt) annot tops)))

(* Test Suite *)

let suite =
  "Source File" >:: test_desug_file
