(* Expressions *)

let rec convert_expr env expr kontinue = match expr with
  | Mono.ExprBuiltin expr -> convert_expr_builtin env expr.fn expr.args kontinue
  | Mono.ExprAtom expr -> convert_expr_atom env expr.atom kontinue

and convert_expr_builtin env fn args kontinue =
  Builtin.convert_builtin env fn (fun fn ->
    let _ = failwith "TODO" in
    let _ = args in
    []
      |> Clos.expr_builtin fn
      |> kontinue Clos.ty_bool)

and convert_expr_atom env atom kontinue =
  Atom.convert_atom env atom (fun ty atom ->
    atom
      |> Clos.expr_atom
      |> kontinue ty)
