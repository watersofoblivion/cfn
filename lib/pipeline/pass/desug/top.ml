(* Top-Level Expressions *)

let rec desug_top env top kontinue = match top with
  | Syntax.TopLet top -> desug_top_let env top.binding kontinue
  | Syntax.TopVal top -> desug_top_val env top.binding kontinue

and desug_top_let env binding kontinue =
  Expr.desug_binding env binding (fun env binding ->
    Annot.top_let binding
      |> kontinue env)

and desug_top_val env binding kontinue =
  Expr.desug_binding env binding (fun env binding ->
    Annot.top_let binding
      |> kontinue env)
