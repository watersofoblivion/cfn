open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Ir.ty * Ir.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

(* Helpers *)

let to_expr = function
  | Ir.BlockExpr block -> block.expr
  | _ -> failwith "Not an expression"

(* Expressions *)

let norm_atom _ constr ty value kontinue =
  value
    |> constr
    |> Ir.expr_atom
    |> Ir.block_expr
    |> kontinue ty

let rec norm_expr env expr kontinue = match expr with
  | Annot.ExprBool expr -> norm_atom env Ir.atom_bool Ir.ty_bool expr.value kontinue
  | Annot.ExprInt expr -> norm_atom env Ir.atom_int Ir.ty_int expr.value kontinue
  | Annot.ExprLong expr -> norm_atom env Ir.atom_long Ir.ty_long expr.value kontinue
  | Annot.ExprFloat expr -> norm_atom env Ir.atom_float Ir.ty_float expr.value kontinue
  | Annot.ExprDouble expr -> norm_atom env Ir.atom_double Ir.ty_double expr.value kontinue
  | Annot.ExprRune expr -> norm_atom env Ir.atom_rune Ir.ty_rune expr.value kontinue
  | Annot.ExprString expr -> norm_atom env Ir.atom_string Ir.ty_string expr.value kontinue
  | Annot.ExprIdent expr -> norm_expr_ident env expr.id kontinue
  | Annot.ExprBuiltin expr -> norm_expr_builtin env expr.fn expr.args kontinue
  | Annot.ExprLet expr -> norm_expr_let env expr.binding expr.scope kontinue

and norm_expr_ident env id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Ir.atom_ident
      |> Ir.expr_atom
      |> Ir.block_expr
      |> kontinue ty
  with Not_found -> unbound_identifier id

and norm_expr_builtin env fn args kontinue =
  Builtin.norm_builtin env fn (fun fn ->
    let _ = failwith "TODO" in
    let _ = args in
    []
      |> Ir.expr_builtin fn
      |> Ir.block_expr
      |> kontinue Ir.ty_bool)

and norm_expr_let _ _ _ _ =
  failwith "TODO"

(* Bindings *)

and norm_binding env binding kontinue = match binding with
  | Annot.Binding binding ->
    norm_expr env binding.value (fun inferred block ->
      Type.norm_ty env binding.ty (fun annotated ->
        if Ir.ty_equal inferred annotated
        then
          Patt.norm_patt env binding.patt inferred (fun env patt ->
            block
              |> to_expr
              |> Ir.binding patt inferred
              |> kontinue env)
        else mismatched_types inferred annotated))

(* Top-Level Expressions *)

let norm_top env top kontinue = match top with
  | Annot.TopLet top ->
    norm_binding env top.binding (fun env binding ->
      Ir.top_let binding
        |> kontinue env)
