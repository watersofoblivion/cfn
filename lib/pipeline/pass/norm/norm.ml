open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Ir.ty * Ir.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let to_expr = function
  | Ir.Expr block -> block.expr

let norm_ty _ ty kontinue = match ty with
  | Annot.TyBool -> kontinue Ir.ty_bool
  | Annot.TyInt -> kontinue Ir.ty_int
  | Annot.TyLong -> kontinue Ir.ty_long
  | Annot.TyFloat -> kontinue Ir.ty_float
  | Annot.TyDouble -> kontinue Ir.ty_double
  | Annot.TyRune -> kontinue Ir.ty_rune
  | Annot.TyString -> kontinue Ir.ty_string

let norm_expr env expr kontinue = match expr with
  | Annot.Bool expr ->
    expr.value
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_bool
  | Annot.Int expr ->
    expr.value
      |> Ir.atom_int
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_int
  | Annot.Long expr ->
    expr.value
      |> Ir.atom_long
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_long
  | Annot.Float expr ->
    expr.value
      |> Ir.atom_float
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_float
  | Annot.Double expr ->
    expr.value
      |> Ir.atom_double
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_double
  | Annot.Rune expr ->
    expr.value
      |> Ir.atom_rune
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_rune
  | Annot.String expr ->
    expr.value
      |> Ir.atom_string
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue Ir.ty_string
  | Annot.Ident expr ->
    try
      let ty = Env.lookup expr.id env in
      expr.id
        |> Ir.atom_ident
        |> Ir.expr_atom
        |> Ir.block_expr
        |> kontinue ty
    with Not_found -> unbound_identifier expr.id

let norm_patt env patt ty kontinue = match patt with
  | Annot.PattGround ->
    Ir.patt_ground
      |> kontinue env
  | Annot.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      Ir.patt_var patt.id
        |> kontinue env)

let norm_binding env binding kontinue = match binding with
  | Annot.Binding binding ->
    norm_expr env binding.value (fun inferred block ->
      norm_ty env binding.ty (fun annotated ->
        if Ir.ty_equal inferred annotated
        then
          norm_patt env binding.patt inferred (fun env patt ->
            block
              |> to_expr
              |> Ir.binding patt inferred
              |> kontinue env)
        else mismatched_types inferred annotated))

let norm_top env top kontinue = match top with
  | Annot.Let top ->
    norm_binding env top.binding (fun env binding ->
      Ir.top_let binding
        |> kontinue env)
