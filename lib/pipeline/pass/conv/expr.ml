(* Expressions *)

let convert_arity args arity kontinue = match arity with
  | Clos.ArityFixed arity ->
    let actual = List.length args in
    if actual = arity.arity
    then kontinue arity.args arity.res
    else Check.invalid_arity arity.arity args
  | Clos.ArityVar arity ->
    let tys = List.map (fun _ -> arity.args) args in
    kontinue tys arity.res

let rec convert_args env args tys kontinue = match args, tys with
  | [], [] -> kontinue []
  | arg :: args, ty :: tys ->
    Atom.convert_atom env arg (fun inferred arg ->
      if Clos.ty_equal ty inferred
      then
        convert_args env args tys (fun args ->
          arg :: args
            |> kontinue)
      else Check.mismatched_types inferred ty)
  | _ ->
    (* Checked in convert_arity *)
    Invalid_argument "Cannot happen"
      |> raise

let rec convert_expr env expr kontinue = match expr with
  | Mono.ExprBuiltin expr -> convert_expr_builtin env expr.fn expr.args kontinue
  | Mono.ExprAtom expr -> convert_expr_atom env expr.atom kontinue

and convert_expr_builtin env fn args kontinue =
  Builtin.convert_builtin env fn (fun arity builtin ->
    convert_arity args arity (fun tys res ->
      convert_args env args tys (fun args ->
        args
          |> Clos.expr_builtin builtin
          |> kontinue res)))

and convert_expr_atom env atom kontinue =
  Atom.convert_atom env atom (fun ty atom ->
    atom
      |> Clos.expr_atom
      |> kontinue ty)
