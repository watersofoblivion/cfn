open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Clos.ty * Clos.ty

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
  | Mono.AtomBool atom -> convert_atom_value Clos.atom_bool Clos.ty_bool atom.value kontinue
  | Mono.AtomInt atom -> convert_atom_value Clos.atom_int Clos.ty_int atom.value kontinue
  | Mono.AtomLong atom -> convert_atom_value Clos.atom_long Clos.ty_long atom.value kontinue
  | Mono.AtomFloat atom -> convert_atom_value Clos.atom_float Clos.ty_float atom.value kontinue
  | Mono.AtomDouble atom -> convert_atom_value Clos.atom_double Clos.ty_double atom.value kontinue
  | Mono.AtomRune atom -> convert_atom_value Clos.atom_rune Clos.ty_rune atom.value kontinue
  | Mono.AtomString atom -> convert_atom_value Clos.atom_string Clos.ty_string atom.value kontinue
  | Mono.AtomIdent atom -> convert_atom_ident env atom.id kontinue

and convert_atom_ident env id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Clos.atom_ident
      |> kontinue ty
  with Not_found -> unbound_identifier id

(* Expressions *)

let rec convert_expr env expr kontinue = match expr with
  | Mono.ExprBuiltin expr -> convert_expr_builtin env expr.fn expr.args kontinue
  | Mono.ExprAtom expr -> convert_expr_atom env expr.atom kontinue

and convert_expr_builtin env fn args kontinue =
  Builtin.convert_builtin env fn (fun fn ->
    let _ = failwith "TODO" in
    let _ = args in
    []
      |> Clos.expr_builtin fn
      |> kontinue Clos.ty_bool)

and convert_expr_atom env atom kontinue =
  convert_atom env atom (fun ty atom ->
    atom
      |> Clos.expr_atom
      |> kontinue ty)

(* Bindings *)

let convert_binding env binding kontinue = match binding with
  | Mono.Binding binding ->
    convert_expr env binding.value (fun inferred value ->
      Type.convert_ty env binding.ty (fun annotated ->
        if Clos.ty_equal inferred annotated
        then
          Patt.convert_patt env binding.patt inferred (fun env patt ->
            Clos.binding patt inferred value
              |> kontinue env)
        else mismatched_types inferred annotated))

(* Terms *)

let rec convert_term env term kontinue = match term with
  | Mono.TermLet term -> convert_term_let env term.binding term.scope kontinue
  | Mono.TermExpr term -> convert_term_expr env term.expr kontinue

and convert_term_let env binding scope kontinue =
  convert_binding env binding (fun env binding ->
    convert_term env scope (fun env scope ->
      Clos.term_let binding scope
        |> kontinue env))

and convert_term_expr env expr kontinue =
    convert_expr env expr (fun ty expr ->
      expr
        |> Clos.term_expr
        |> kontinue ty)

(* Top-Level Expressions *)

let convert_top env top kontinue = match top with
  | Mono.TopLet top ->
    convert_binding env top.binding (fun env binding ->
      Clos.top_let binding
        |> kontinue env)
