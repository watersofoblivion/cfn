(* Expressions *)

let mono_arity args arity kontinue = match arity with
  | Mono.ArityFixed arity ->
    let actual = List.length args in
    if actual = arity.arity
    then kontinue arity.args arity.res
    else Check.invalid_arity arity.arity args
  | Mono.ArityVar arity ->
    let tys = List.map (fun _ -> arity.args) args in
    kontinue tys arity.res

let rec mono_args env args tys kontinue = match args, tys with
  | [], [] -> kontinue []
  | arg :: args, ty :: tys ->
    Atom.mono_atom env arg (fun inferred arg ->
      if Mono.ty_equal ty inferred
      then
        mono_args env args tys (fun args ->
          arg :: args
            |> kontinue)
      else Check.mismatched_types inferred ty)
  | _ ->
    (* Checked in mono_arity *)
    Invalid_argument "Cannot happen"
      |> raise

let rec mono_expr env expr kontinue = match expr with
  | Ir.ExprBuiltin expr -> mono_expr_builtin env expr.fn expr.args kontinue
  | Ir.ExprAtom expr -> mono_expr_atom env expr.atom kontinue

and mono_expr_builtin env fn args kontinue =
  Builtin.mono_builtin env fn (fun arity builtin ->
    mono_arity args arity (fun tys res ->
      mono_args env args tys (fun args ->
        args
          |> Mono.expr_builtin builtin
          |> kontinue res)))

and mono_expr_atom env atom kontinue =
  Atom.mono_atom env atom (fun ty atom ->
    atom
      |> Mono.expr_atom
      |> kontinue ty)
