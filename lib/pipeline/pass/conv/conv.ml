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

(* Types *)

let convert_ty _ ty kontinue = match ty with
  | Mono.TyBool -> kontinue Clos.ty_bool
  | Mono.TyInt -> kontinue Clos.ty_int
  | Mono.TyLong -> kontinue Clos.ty_long
  | Mono.TyFloat -> kontinue Clos.ty_float
  | Mono.TyDouble -> kontinue Clos.ty_double
  | Mono.TyRune -> kontinue Clos.ty_rune
  | Mono.TyString -> kontinue Clos.ty_string

(* Builtins *)

let convert_builtin_un constr env ty kontinue =
  convert_ty env ty (fun ty ->
    constr ty
      |> kontinue)

let rec convert_builtin env builtin kontinue = match builtin with
  | Mono.Add builtin -> convert_builtin_un Clos.builtin_add env builtin.ty kontinue
  | Mono.Sub builtin -> convert_builtin_un Clos.builtin_sub env builtin.ty kontinue
  | Mono.Mul builtin -> convert_builtin_un Clos.builtin_mul env builtin.ty kontinue
  | Mono.Div builtin -> convert_builtin_un Clos.builtin_div env builtin.ty kontinue
  | Mono.Mod builtin -> convert_builtin_un Clos.builtin_mod env builtin.ty kontinue
  | Mono.Exp builtin -> convert_builtin_un Clos.builtin_exp env builtin.ty kontinue
  | Mono.Promote builtin -> convert_builtin_promote env builtin.sub builtin.sup kontinue
  | Mono.Concat builtin -> convert_builtin_un Clos.builtin_concat env builtin.ty kontinue

and convert_builtin_promote env sub sup kontinue =
  convert_ty env sub (fun sub ->
    convert_ty env sup (fun sup ->
      Clos.builtin_promote sub sup
        |> kontinue))

(* Atoms *)

let convert_atom_value constr ty value kontinue =
  value
    |> constr
    |> kontinue ty

let rec convert_atom env atom kontinue = match atom with
  | Mono.Bool atom -> convert_atom_value Clos.atom_bool Clos.ty_bool atom.value kontinue
  | Mono.Int atom -> convert_atom_value Clos.atom_int Clos.ty_int atom.value kontinue
  | Mono.Long atom -> convert_atom_value Clos.atom_long Clos.ty_long atom.value kontinue
  | Mono.Float atom -> convert_atom_value Clos.atom_float Clos.ty_float atom.value kontinue
  | Mono.Double atom -> convert_atom_value Clos.atom_double Clos.ty_double atom.value kontinue
  | Mono.Rune atom -> convert_atom_value Clos.atom_rune Clos.ty_rune atom.value kontinue
  | Mono.String atom -> convert_atom_value Clos.atom_string Clos.ty_string atom.value kontinue
  | Mono.Ident atom -> convert_atom_ident env atom.id kontinue

and convert_atom_ident env id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Clos.atom_ident
      |> kontinue ty
  with Not_found -> unbound_identifier id

(* Expressions *)

let rec convert_expr env expr kontinue = match expr with
  | Mono.Builtin expr -> convert_expr_builtin env expr.fn expr.args kontinue
  | Mono.Atom expr -> convert_expr_atom env expr.atom kontinue

and convert_expr_builtin env fn args kontinue =
  convert_builtin env fn (fun fn ->
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

(* Patterns *)

let rec convert_patt env patt ty kontinue = match patt with
  | Mono.PattGround -> convert_patt_ground env kontinue
  | Mono.PattVar patt -> convert_patt_var env patt.id ty kontinue

and convert_patt_ground env kontinue =
  Clos.patt_ground
    |> kontinue env

and convert_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    id
      |> Clos.patt_var
      |> kontinue env)

(* Bindings *)

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

(* Blocks *)

let rec convert_block env block kontinue = match block with
  | Mono.Bind block -> convert_block_bind env block.binding block.scope kontinue
  | Mono.Expr block -> convert_block_expr env block.expr kontinue

and convert_block_bind env binding scope kontinue =
  convert_binding env binding (fun env binding ->
    convert_block env scope (fun env scope ->
      Clos.block_bind binding scope
        |> kontinue env))

and convert_block_expr env expr kontinue =
    convert_expr env expr (fun ty expr ->
      expr
        |> Clos.block_expr
        |> kontinue ty)

(* Top-Level Expressions *)

let convert_top env top kontinue = match top with
  | Mono.Let top ->
    convert_binding env top.binding (fun env binding ->
      Clos.top_let binding
        |> kontinue env)
