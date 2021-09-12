(* Top-Level Expressions *)

let convert_top env top kontinue = match top with
  | Mono.TopLet top ->
    Term.convert_binding env top.binding (fun env binding ->
      Clos.top_let binding
        |> kontinue env)
