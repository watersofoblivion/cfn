let rec convert_expr env expr kontinue = match expr with
  | Ir.ExprBuiltin expr -> convert_expr_builtin env expr.fn expr.args kontinue
  | Ir.ExprAtom expr -> convert_expr_atom env expr.atom kontinue

and convert_expr_builtin env fn args kontinue =
  Builtin.convert_builtin env fn (fun fn ->
    let _ = failwith "TODO" in
    let _ = args in
    []
      |> Mono.expr_builtin fn
      |> kontinue Mono.ty_bool)

and convert_expr_atom env atom kontinue =
  Atom.convert_atom env atom (fun ty atom ->
    atom
      |> Mono.expr_atom
      |> kontinue ty)
