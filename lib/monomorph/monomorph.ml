let convert_ty _ ty kontinue = match ty with
  | Ir.Type.Bool ->
    Mono.Type.bool
      |> kontinue
  | Ir.Type.Int ->
    Mono.Type.int
      |> kontinue
  | Ir.Type.Long ->
    Mono.Type.long
      |> kontinue
  | Ir.Type.Float ->
    Mono.Type.float
      |> kontinue
  | Ir.Type.Double ->
    Mono.Type.double
      |> kontinue
  | Ir.Type.Rune ->
    Mono.Type.rune
      |> kontinue
  | Ir.Type.String ->
    Mono.Type.string
      |> kontinue

let convert_atom _ atom kontinue = match atom with
  | Ir.Ast.Bool atom ->
    atom.value
      |> Mono.Ast.atom_bool
      |> kontinue
  | Ir.Ast.Int atom ->
    atom.value
      |> Mono.Ast.atom_int
      |> kontinue
  | Ir.Ast.Long atom ->
    atom.value
      |> Mono.Ast.atom_long
      |> kontinue
  | Ir.Ast.Float atom ->
    atom.value
      |> Mono.Ast.atom_float
      |> kontinue
  | Ir.Ast.Double atom ->
    atom.value
      |> Mono.Ast.atom_double
      |> kontinue
  | Ir.Ast.Rune atom ->
    atom.value
      |> Mono.Ast.atom_rune
      |> kontinue
  | Ir.Ast.String atom ->
    atom.value
      |> Mono.Ast.atom_string
      |> kontinue
  | Ir.Ast.Ident atom ->
    atom.id
      |> Mono.Ast.atom_ident
      |> kontinue

let convert_expr env expr kontinue = match expr with
  | Ir.Ast.Atom expr ->
    convert_atom env expr.atom (fun atom ->
      Mono.Ast.expr_atom atom
       |> kontinue)

let convert_block env block kontinue = match block with
  | Ir.Ast.Expr block ->
    convert_expr env block.expr (fun expr ->
      Mono.Ast.block_expr expr
        |> kontinue)

let convert_patt _ patt kontinue = match patt with
  | Ir.Ast.PattGround ->
    Mono.Ast.patt_ground
      |> kontinue
  | Ir.Ast.PattVar patt ->
    patt.id
      |> Mono.Ast.patt_var
      |> kontinue

let convert_binding env binding kontinue = match binding with
  | Ir.Ast.Binding binding ->
    convert_expr env binding.value (fun value ->
      convert_ty env binding.ty (fun ty ->
        convert_patt env binding.patt (fun patt ->
          Mono.Ast.binding patt ty value
            |> kontinue)))

let convert_top env top kontinue = match top with
  | Ir.Ast.Let top ->
    convert_binding env top.binding (fun binding ->
      Mono.Ast.top_let binding
        |> kontinue)
