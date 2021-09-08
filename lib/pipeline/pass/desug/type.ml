(* Types *)

open Common

(* Exceptions *)

exception UnboundConstructor of Loc.t * Sym.t

let unbound_constructor loc id =
  UnboundConstructor (loc, id)
    |> raise

(* Pass *)

let with_constructors env kontinue =
  let constrs = [
    (Prim.id_bool, Annot.ty_bool);
    (Prim.id_int, Annot.ty_int);
    (Prim.id_long, Annot.ty_long);
    (Prim.id_float, Annot.ty_float);
    (Prim.id_double, Annot.ty_double);
    (Prim.id_rune, Annot.ty_rune);
    (Prim.id_string, Annot.ty_string);
  ] in
  let fold env constr = Env.constr_of constr env (fun env _ -> env) in
  let env =
    constrs
      |> List.map fst
      |> List.fold_left fold env
  in
  let map (constr, annot) = Env.constr_of constr env (fun _ sym -> (sym, annot)) in
  List.map map constrs
    |> kontinue

let desug_ty env ty kontinue =
  with_constructors env (fun mappings ->
    match ty with
      | Syntax.TyConstr constr ->
        try
          List.assoc constr.id mappings
            |> kontinue
        with Not_found -> unbound_constructor constr.loc constr.id)
