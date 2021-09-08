open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Mono.ty * Mono.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

(* Atoms *)

let convert_atom_value constr ty value kontinue =
  value
    |> constr
    |> kontinue ty

let rec convert_atom env atom kontinue = match atom with
  | Ir.AtomBool atom -> convert_atom_value Mono.atom_bool Mono.ty_bool atom.value kontinue
  | Ir.AtomInt atom -> convert_atom_value Mono.atom_int Mono.ty_int atom.value kontinue
  | Ir.AtomLong atom -> convert_atom_value Mono.atom_long Mono.ty_long atom.value kontinue
  | Ir.AtomFloat atom -> convert_atom_value Mono.atom_float Mono.ty_float atom.value kontinue
  | Ir.AtomDouble atom -> convert_atom_value Mono.atom_double Mono.ty_double atom.value kontinue
  | Ir.AtomRune atom -> convert_atom_value Mono.atom_rune Mono.ty_rune atom.value kontinue
  | Ir.AtomString atom -> convert_atom_value Mono.atom_string Mono.ty_string atom.value kontinue
  | Ir.AtomIdent atom -> convert_atom_ident env atom.id kontinue

and convert_atom_ident env id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Mono.atom_ident
      |> kontinue ty
  with Not_found -> unbound_identifier id

(* Expressions *)

let rec convert_expr env expr kontinue = match expr with
  | Ir.ExprBuiltin expr -> convert_expr_builtin env expr.fn expr.args kontinue
  | Ir.ExprAtom expr -> convert_expr_atom env expr.atom kontinue

and convert_expr_builtin env fn args kontinue =
  Builtin.convert_builtin env fn (fun fn ->
    let _ = failwith "TODO" in
    let _ = args in
    []
      |> Mono.expr_builtin fn
      |> kontinue Mono.ty_bool)

and convert_expr_atom env atom kontinue =
  convert_atom env atom (fun ty atom ->
    atom
      |> Mono.expr_atom
      |> kontinue ty)

(* Bindings *)

let convert_binding env binding kontinue = match binding with
  | Ir.Binding binding ->
    convert_expr env binding.value (fun inferred value ->
      Type.convert_ty env binding.ty (fun annotated ->
        if Mono.ty_equal inferred annotated
        then
          Patt.convert_patt env binding.patt inferred (fun env patt ->
            Mono.binding patt inferred value
              |> kontinue env)
        else mismatched_types inferred annotated))

(* Blocks *)

let rec convert_block env block kontinue = match block with
  | Ir.BlockLet block -> convert_block_let env block.binding block.scope kontinue
  | Ir.BlockExpr block -> convert_block_expr env block.expr kontinue

and convert_block_let env binding scope kontinue =
  convert_binding env binding (fun env binding ->
    convert_block env scope (fun env scope ->
      Mono.block_let binding scope
        |> kontinue env))

and convert_block_expr env expr kontinue =
    convert_expr env expr (fun ty expr ->
      expr
        |> Mono.block_expr
        |> kontinue ty)

(* Top-Level Expressions *)

let convert_top env top kontinue = match top with
  | Ir.TopLet top ->
    convert_binding env top.binding (fun env binding ->
      Mono.top_let binding
        |> kontinue env)
