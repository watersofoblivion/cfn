open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Clos.ty * Clos.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let convert_ty _ ty kontinue = match ty with
  | Mono.TyBool -> kontinue Clos.ty_bool
  | Mono.TyInt -> kontinue Clos.ty_int
  | Mono.TyLong -> kontinue Clos.ty_long
  | Mono.TyFloat -> kontinue Clos.ty_float
  | Mono.TyDouble -> kontinue Clos.ty_double
  | Mono.TyRune -> kontinue Clos.ty_rune
  | Mono.TyString -> kontinue Clos.ty_string

let convert_atom env atom kontinue = match atom with
  | Mono.Bool atom ->
    atom.value
      |> Clos.atom_bool
      |> kontinue Clos.ty_bool
  | Mono.Int atom ->
    atom.value
      |> Clos.atom_int
      |> kontinue Clos.ty_int
  | Mono.Long atom ->
    atom.value
      |> Clos.atom_long
      |> kontinue Clos.ty_long
  | Mono.Float atom ->
    atom.value
      |> Clos.atom_float
      |> kontinue Clos.ty_float
  | Mono.Double atom ->
    atom.value
      |> Clos.atom_double
      |> kontinue Clos.ty_double
  | Mono.Rune atom ->
    atom.value
      |> Clos.atom_rune
      |> kontinue Clos.ty_rune
  | Mono.String atom ->
    atom.value
      |> Clos.atom_string
      |> kontinue Clos.ty_string
  | Mono.Ident atom ->
    try
      let ty = Env.lookup atom.id env in
      atom.id
        |> Clos.atom_ident
        |> kontinue ty
    with Not_found ->
      unbound_identifier atom.id

let convert_expr env expr kontinue = match expr with
  | Mono.Atom expr ->
    convert_atom env expr.atom (fun ty atom ->
      atom
        |> Clos.expr_atom
        |> kontinue ty)

let convert_block env block kontinue = match block with
  | Mono.Expr block ->
    convert_expr env block.expr (fun ty expr ->
      expr
        |> Clos.block_expr
        |> kontinue ty)

let convert_patt env patt ty kontinue = match patt with
  | Mono.PattGround ->
    Clos.patt_ground
      |> kontinue env
  | Mono.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      patt.id
        |> Clos.patt_var
        |> kontinue env)

let convert_binding env binding kontinue = match binding with
  | Mono.Binding binding ->
    convert_expr env binding.value (fun inferred value ->
      convert_ty env binding.ty (fun annotated ->
        if Clos.ty_equal inferred annotated
        then
          convert_patt env binding.patt inferred (fun env patt ->
            Clos.binding patt inferred value
              |> kontinue env)
        else mismatched_types inferred annotated))

let convert_top env top kontinue = match top with
  | Mono.Let top ->
    convert_binding env top.binding (fun env binding ->
      Clos.top_let binding
        |> kontinue env)
