(* Bindings *)

let mono_binding env binding kontinue = match binding with
  | Ir.Binding binding ->
    Expr.mono_expr env binding.value (fun inferred value ->
      Type.mono_ty env binding.ty (fun annotated ->
        if Mono.ty_equal inferred annotated
        then
          Patt.mono_patt env binding.patt inferred (fun env patt ->
            Mono.binding patt inferred value
              |> kontinue env)
        else Check.mismatched_types inferred annotated))

(* Terms *)

let rec mono_term env term kontinue = match term with
  | Ir.TermLet term -> mono_term_let env term.binding term.scope kontinue
  | Ir.TermExpr term -> mono_term_expr env term.expr kontinue

and mono_term_let env binding scope kontinue =
  mono_binding env binding (fun env binding ->
    mono_term env scope (fun env scope ->
      Mono.term_let binding scope
        |> kontinue env))

and mono_term_expr env expr kontinue =
  Expr.mono_expr env expr (fun ty expr ->
    expr
      |> Mono.term_expr
      |> kontinue ty)
