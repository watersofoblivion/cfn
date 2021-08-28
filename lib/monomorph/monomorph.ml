open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Mono.Type.t * Mono.Type.t

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let convert_ty _ ty kontinue = match ty with
  | Ir.Type.Bool -> kontinue Mono.Type.bool
  | Ir.Type.Int -> kontinue Mono.Type.int
  | Ir.Type.Long -> kontinue Mono.Type.long
  | Ir.Type.Float -> kontinue Mono.Type.float
  | Ir.Type.Double -> kontinue Mono.Type.double
  | Ir.Type.Rune -> kontinue Mono.Type.rune
  | Ir.Type.String -> kontinue Mono.Type.string

let convert_atom env atom kontinue = match atom with
  | Ir.Ast.Bool atom ->
    atom.value
      |> Mono.Ast.atom_bool
      |> kontinue Mono.Type.bool
  | Ir.Ast.Int atom ->
    atom.value
      |> Mono.Ast.atom_int
      |> kontinue Mono.Type.int
  | Ir.Ast.Long atom ->
    atom.value
      |> Mono.Ast.atom_long
      |> kontinue Mono.Type.long
  | Ir.Ast.Float atom ->
    atom.value
      |> Mono.Ast.atom_float
      |> kontinue Mono.Type.float
  | Ir.Ast.Double atom ->
    atom.value
      |> Mono.Ast.atom_double
      |> kontinue Mono.Type.double
  | Ir.Ast.Rune atom ->
    atom.value
      |> Mono.Ast.atom_rune
      |> kontinue Mono.Type.rune
  | Ir.Ast.String atom ->
    atom.value
      |> Mono.Ast.atom_string
      |> kontinue Mono.Type.string
  | Ir.Ast.Ident atom ->
    try
      let ty = Env.lookup atom.id env in
      atom.id
        |> Mono.Ast.atom_ident
        |> kontinue ty
    with Not_found -> unbound_identifier atom.id

let convert_expr env expr kontinue = match expr with
  | Ir.Ast.Atom expr ->
    convert_atom env expr.atom (fun ty atom ->
      atom
        |> Mono.Ast.expr_atom
        |> kontinue ty)

let convert_block env block kontinue = match block with
  | Ir.Ast.Expr block ->
    convert_expr env block.expr (fun ty expr ->
      expr
        |> Mono.Ast.block_expr
        |> kontinue ty)

let convert_patt env patt ty kontinue = match patt with
  | Ir.Ast.PattGround ->
    Mono.Ast.patt_ground
      |> kontinue env
  | Ir.Ast.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      patt.id
        |> Mono.Ast.patt_var
        |> kontinue env)

let convert_binding env binding kontinue = match binding with
  | Ir.Ast.Binding binding ->
    convert_expr env binding.value (fun inferred value ->
      convert_ty env binding.ty (fun annotated ->
        if Mono.Type.equal inferred annotated
        then
          convert_patt env binding.patt inferred (fun env patt ->
            Mono.Ast.binding patt inferred value
              |> kontinue env)
        else mismatched_types inferred annotated))

let convert_top env top kontinue = match top with
  | Ir.Ast.Let top ->
    convert_binding env top.binding (fun env binding ->
      Mono.Ast.top_let binding
        |> kontinue env)
