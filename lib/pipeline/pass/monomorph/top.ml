let convert_top env top kontinue = match top with
  | Ir.TopLet top ->
    Term.convert_binding env top.binding (fun env binding ->
      Mono.top_let binding
        |> kontinue env)
