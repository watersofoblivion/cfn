open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Clos.Type.t * Clos.Type.t

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let convert_ty _ ty kontinue = match ty with
  | Mono.Type.Bool -> kontinue Clos.Type.bool
  | Mono.Type.Int -> kontinue Clos.Type.int
  | Mono.Type.Long -> kontinue Clos.Type.long
  | Mono.Type.Float -> kontinue Clos.Type.float
  | Mono.Type.Double -> kontinue Clos.Type.double
  | Mono.Type.Rune -> kontinue Clos.Type.rune
  | Mono.Type.String -> kontinue Clos.Type.string

let convert_atom env atom kontinue = match atom with
  | Mono.Ast.Bool atom ->
    atom.value
      |> Clos.Ast.atom_bool
      |> kontinue Clos.Type.bool
  | Mono.Ast.Int atom ->
    atom.value
      |> Clos.Ast.atom_int
      |> kontinue Clos.Type.int
  | Mono.Ast.Long atom ->
    atom.value
      |> Clos.Ast.atom_long
      |> kontinue Clos.Type.long
  | Mono.Ast.Float atom ->
    atom.value
      |> Clos.Ast.atom_float
      |> kontinue Clos.Type.float
  | Mono.Ast.Double atom ->
    atom.value
      |> Clos.Ast.atom_double
      |> kontinue Clos.Type.double
  | Mono.Ast.Rune atom ->
    atom.value
      |> Clos.Ast.atom_rune
      |> kontinue Clos.Type.rune
  | Mono.Ast.String atom ->
    atom.value
      |> Clos.Ast.atom_string
      |> kontinue Clos.Type.string
  | Mono.Ast.Ident atom ->
    try
      let ty = Env.lookup atom.id env in
      atom.id
        |> Clos.Ast.atom_ident
        |> kontinue ty
    with Not_found ->
      unbound_identifier atom.id

let convert_expr env expr kontinue = match expr with
  | Mono.Ast.Atom expr ->
    convert_atom env expr.atom (fun ty atom ->
      atom
        |> Clos.Ast.expr_atom
        |> kontinue ty)

let convert_block env block kontinue = match block with
  | Mono.Ast.Expr block ->
    convert_expr env block.expr (fun ty expr ->
      expr
        |> Clos.Ast.block_expr
        |> kontinue ty)

let convert_patt env patt ty kontinue = match patt with
  | Mono.Ast.PattGround ->
    Clos.Ast.patt_ground
      |> kontinue env
  | Mono.Ast.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      patt.id
        |> Clos.Ast.patt_var
        |> kontinue env)

let convert_binding env binding kontinue = match binding with
  | Mono.Ast.Binding binding ->
    convert_expr env binding.value (fun inferred value ->
      convert_ty env binding.ty (fun annotated ->
        if Clos.Type.equal inferred annotated
        then
          convert_patt env binding.patt inferred (fun env patt ->
            Clos.Ast.binding patt inferred value
              |> kontinue env)
        else mismatched_types inferred annotated))

let convert_top env top kontinue = match top with
  | Mono.Ast.Let top ->
    convert_binding env top.binding (fun env binding ->
      Clos.Ast.top_let binding
        |> kontinue env)
