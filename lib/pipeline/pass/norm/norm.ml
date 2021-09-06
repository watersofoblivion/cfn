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

(* Types *)

let norm_ty _ ty kontinue = match ty with
  | Annot.TyBool -> kontinue Ir.ty_bool
  | Annot.TyInt -> kontinue Ir.ty_int
  | Annot.TyLong -> kontinue Ir.ty_long
  | Annot.TyFloat -> kontinue Ir.ty_float
  | Annot.TyDouble -> kontinue Ir.ty_double
  | Annot.TyRune -> kontinue Ir.ty_rune
  | Annot.TyString -> kontinue Ir.ty_string

(* Builtins *)

let norm_builtin_un constr env ty kontinue =
  norm_ty env ty (fun ty ->
    constr ty
      |> kontinue)

let rec norm_builtin env builtin kontinue = match builtin with
  | Annot.BuiltinAdd builtin -> norm_builtin_un Ir.builtin_add env builtin.ty kontinue
  | Annot.BuiltinSub builtin -> norm_builtin_un Ir.builtin_sub env builtin.ty kontinue
  | Annot.BuiltinMul builtin -> norm_builtin_un Ir.builtin_mul env builtin.ty kontinue
  | Annot.BuiltinDiv builtin -> norm_builtin_un Ir.builtin_div env builtin.ty kontinue
  | Annot.BuiltinMod builtin -> norm_builtin_un Ir.builtin_mod env builtin.ty kontinue
  | Annot.BuiltinExp builtin -> norm_builtin_un Ir.builtin_exp env builtin.ty kontinue
  | Annot.BuiltinPromote builtin -> norm_builtin_promote env builtin.sub builtin.sup kontinue
  | Annot.BuiltinConcat builtin -> norm_builtin_un Ir.builtin_concat env builtin.ty kontinue

and norm_builtin_promote env sub sup kontinue =
  norm_ty env sub (fun sub ->
    norm_ty env sup (fun sup ->
      Ir.builtin_promote sub sup
        |> kontinue))

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
  norm_builtin env fn (fun fn ->
    let _ = failwith "TODO" in
    let _ = args in
    []
      |> Ir.expr_builtin fn
      |> Ir.block_expr
      |> kontinue Ir.ty_bool)

(* Patterns *)

let rec norm_patt env patt ty kontinue = match patt with
  | Annot.PattGround -> norm_patt_ground env kontinue
  | Annot.PattVar patt -> norm_patt_var env patt.id ty kontinue

and norm_patt_ground env kontinue =
  Ir.patt_ground
    |> kontinue env

and norm_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    Ir.patt_var id
      |> kontinue env)

(* Bindings *)

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

(* Top-Level Expressions *)

let norm_top env top kontinue = match top with
  | Annot.TopLet top ->
    norm_binding env top.binding (fun env binding ->
      Ir.top_let binding
        |> kontinue env)
