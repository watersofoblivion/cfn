open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Mono.ty * Mono.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let convert_ty _ ty kontinue = match ty with
  | Ir.TyBool -> kontinue Mono.ty_bool
  | Ir.TyInt -> kontinue Mono.ty_int
  | Ir.TyLong -> kontinue Mono.ty_long
  | Ir.TyFloat -> kontinue Mono.ty_float
  | Ir.TyDouble -> kontinue Mono.ty_double
  | Ir.TyRune -> kontinue Mono.ty_rune
  | Ir.TyString -> kontinue Mono.ty_string

let convert_atom env atom kontinue = match atom with
  | Ir.Bool atom ->
    atom.value
      |> Mono.atom_bool
      |> kontinue Mono.ty_bool
  | Ir.Int atom ->
    atom.value
      |> Mono.atom_int
      |> kontinue Mono.ty_int
  | Ir.Long atom ->
    atom.value
      |> Mono.atom_long
      |> kontinue Mono.ty_long
  | Ir.Float atom ->
    atom.value
      |> Mono.atom_float
      |> kontinue Mono.ty_float
  | Ir.Double atom ->
    atom.value
      |> Mono.atom_double
      |> kontinue Mono.ty_double
  | Ir.Rune atom ->
    atom.value
      |> Mono.atom_rune
      |> kontinue Mono.ty_rune
  | Ir.String atom ->
    atom.value
      |> Mono.atom_string
      |> kontinue Mono.ty_string
  | Ir.Ident atom ->
    try
      let ty = Env.lookup atom.id env in
      atom.id
        |> Mono.atom_ident
        |> kontinue ty
    with Not_found -> unbound_identifier atom.id

let convert_expr env expr kontinue = match expr with
  | Ir.Atom expr ->
    convert_atom env expr.atom (fun ty atom ->
      atom
        |> Mono.expr_atom
        |> kontinue ty)

let convert_block env block kontinue = match block with
  | Ir.Expr block ->
    convert_expr env block.expr (fun ty expr ->
      expr
        |> Mono.block_expr
        |> kontinue ty)

let convert_patt env patt ty kontinue = match patt with
  | Ir.PattGround ->
    Mono.patt_ground
      |> kontinue env
  | Ir.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      patt.id
        |> Mono.patt_var
        |> kontinue env)

let convert_binding env binding kontinue = match binding with
  | Ir.Binding binding ->
    convert_expr env binding.value (fun inferred value ->
      convert_ty env binding.ty (fun annotated ->
        if Mono.ty_equal inferred annotated
        then
          convert_patt env binding.patt inferred (fun env patt ->
            Mono.binding patt inferred value
              |> kontinue env)
        else mismatched_types inferred annotated))

let convert_top env top kontinue = match top with
  | Ir.Let top ->
    convert_binding env top.binding (fun env binding ->
      Mono.top_let binding
        |> kontinue env)
