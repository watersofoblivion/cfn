open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Ir.Type.t * Ir.Type.t

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let to_expr = function
  | Ir.Ast.Expr block -> block.expr

let norm_ty _ ty kontinue = match ty with
  | Annot.Type.Bool -> kontinue Ir.Type.bool
  | Annot.Type.Int -> kontinue Ir.Type.int
  | Annot.Type.Long -> kontinue Ir.Type.long
  | Annot.Type.Float -> kontinue Ir.Type.float
  | Annot.Type.Double -> kontinue Ir.Type.double
  | Annot.Type.Rune -> kontinue Ir.Type.rune
  | Annot.Type.String -> kontinue Ir.Type.string

let norm_expr env expr kontinue = match expr with
  | Annot.Ast.Bool expr ->
    expr.value
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.bool
  | Annot.Ast.Int expr ->
    expr.value
      |> Ir.Ast.atom_int
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.int
  | Annot.Ast.Long expr ->
    expr.value
      |> Ir.Ast.atom_long
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.long
  | Annot.Ast.Float expr ->
    expr.value
      |> Ir.Ast.atom_float
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.float
  | Annot.Ast.Double expr ->
    expr.value
      |> Ir.Ast.atom_double
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.double
  | Annot.Ast.Rune expr ->
    expr.value
      |> Ir.Ast.atom_rune
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.rune
  | Annot.Ast.String expr ->
    expr.value
      |> Ir.Ast.atom_string
      |> Ir.Ast.expr_atom
      |> Ir.Ast.block_expr
      |> kontinue Ir.Type.string
  | Annot.Ast.Ident expr ->
    try
      let ty = Env.lookup expr.id env in
      expr.id
        |> Ir.Ast.atom_ident
        |> Ir.Ast.expr_atom
        |> Ir.Ast.block_expr
        |> kontinue ty
    with Not_found -> unbound_identifier expr.id

let norm_patt env patt ty kontinue = match patt with
  | Annot.Ast.PattGround ->
    Ir.Ast.patt_ground
      |> kontinue env
  | Annot.Ast.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      Ir.Ast.patt_var patt.id
        |> kontinue env)

let norm_binding env binding kontinue = match binding with
  | Annot.Ast.Binding binding ->
    norm_expr env binding.value (fun inferred block ->
      norm_ty env binding.ty (fun annotated ->
        if Ir.Type.equal inferred annotated
        then
          norm_patt env binding.patt inferred (fun env patt ->
            block
              |> to_expr
              |> Ir.Ast.binding patt inferred
              |> kontinue env)
        else mismatched_types inferred annotated))

let norm_top env top kontinue = match top with
  | Annot.Ast.Let top ->
    norm_binding env top.binding (fun env binding ->
      Ir.Ast.top_let binding
        |> kontinue env)
