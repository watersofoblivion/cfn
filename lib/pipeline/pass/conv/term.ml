(* Terms *)

(* Bindings *)

let convert_binding env binding kontinue = match binding with
  | Mono.Binding binding ->
    Expr.convert_expr env binding.value (fun inferred value ->
      Type.convert_ty env binding.ty (fun annotated ->
        if Clos.ty_equal inferred annotated
        then
          Patt.convert_patt env binding.patt inferred (fun env patt ->
            Clos.binding patt inferred value
              |> kontinue env)
        else Check.mismatched_types inferred annotated))

(* Terms *)

let rec convert_term env term kontinue = match term with
  | Mono.TermLet term -> convert_term_let env term.binding term.scope kontinue
  | Mono.TermExpr term -> convert_term_expr env term.expr kontinue

and convert_term_let env binding scope kontinue =
  convert_binding env binding (fun env binding ->
    convert_term env scope (fun env scope ->
      Clos.term_let binding scope
        |> kontinue env))

and convert_term_expr env expr kontinue =
  Expr.convert_expr env expr (fun ty expr ->
    expr
      |> Clos.term_expr
      |> kontinue ty)