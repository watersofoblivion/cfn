let norm_top env top kontinue = match top with
  | Annot.TopLet top ->
    Expr.norm_binding env top.binding (fun env binding ->
      Ir.top_let binding
        |> kontinue env)
