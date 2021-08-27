let convert_ty _ ty kontinue = match ty with
  | Mono.Type.Bool ->
    Clos.Type.bool
      |> kontinue
  | Mono.Type.Int ->
    Clos.Type.int
      |> kontinue
  | Mono.Type.Long ->
    Clos.Type.long
      |> kontinue
  | Mono.Type.Float ->
    Clos.Type.float
      |> kontinue
  | Mono.Type.Double ->
    Clos.Type.double
      |> kontinue
  | Mono.Type.Rune ->
    Clos.Type.rune
      |> kontinue
  | Mono.Type.String ->
    Clos.Type.string
      |> kontinue

let convert_atom _ atom kontinue = match atom with
  | Mono.Ast.Bool atom ->
    atom.value
      |> Clos.Ast.atom_bool
      |> kontinue
  | Mono.Ast.Int atom ->
    atom.value
      |> Clos.Ast.atom_int
      |> kontinue
  | Mono.Ast.Long atom ->
    atom.value
      |> Clos.Ast.atom_long
      |> kontinue
  | Mono.Ast.Float atom ->
    atom.value
      |> Clos.Ast.atom_float
      |> kontinue
  | Mono.Ast.Double atom ->
    atom.value
      |> Clos.Ast.atom_double
      |> kontinue
  | Mono.Ast.Rune atom ->
    atom.value
      |> Clos.Ast.atom_rune
      |> kontinue
  | Mono.Ast.String atom ->
    atom.value
      |> Clos.Ast.atom_string
      |> kontinue
  | Mono.Ast.Ident atom ->
    atom.id
      |> Clos.Ast.atom_ident
      |> kontinue

let convert_expr env expr kontinue = match expr with
  | Mono.Ast.Atom expr ->
    convert_atom env expr.atom (fun atom ->
      Clos.Ast.expr_atom atom
       |> kontinue)

let convert_block env block kontinue = match block with
  | Mono.Ast.Expr block ->
    convert_expr env block.expr (fun expr ->
      Clos.Ast.block_expr expr
        |> kontinue)

let convert_patt _ patt kontinue = match patt with
  | Mono.Ast.PattGround ->
    Clos.Ast.patt_ground
      |> kontinue
  | Mono.Ast.PattVar patt ->
    patt.id
      |> Clos.Ast.patt_var
      |> kontinue

let convert_binding env binding kontinue = match binding with
  | Mono.Ast.Binding binding ->
    convert_expr env binding.value (fun value ->
      convert_ty env binding.ty (fun ty ->
        convert_patt env binding.patt (fun patt ->
          Clos.Ast.binding patt ty value
            |> kontinue)))

let convert_top env top kontinue = match top with
  | Mono.Ast.Let top ->
    convert_binding env top.binding (fun binding ->
      Clos.Ast.top_let binding
        |> kontinue)
