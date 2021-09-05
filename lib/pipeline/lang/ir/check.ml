(* Type Checking *)

open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Type.ty * Type.ty
exception InvalidArity of int * int
exception UnsupportedConcatArg of Ast.atom * Type.ty * Type.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let invalid_arity expected lst =
  let actual = List.length lst in
  InvalidArity (expected, actual)
    |> raise

let unsupported_concat_arg arg inferred expected =
  UnsupportedConcatArg (arg, inferred, expected)
    |> raise

(* Built-in Functions *)

let check_builtin_binop test check err env ty args kontinue =
  if test ty
  then
    match args with
      | lhs :: rhs :: [] -> check env ty lhs rhs kontinue
      | _ -> invalid_arity 2 args
  else err ty

let rec check_builtin env fn args kontinue = match fn with
  | Builtin.Add builtin -> check_builtin_binop_numeric env builtin.ty args kontinue
  | Builtin.Sub builtin -> check_builtin_binop_numeric env builtin.ty args kontinue
  | Builtin.Mul builtin -> check_builtin_binop_numeric env builtin.ty args kontinue
  | Builtin.Div builtin -> check_builtin_binop_numeric env builtin.ty args kontinue
  | Builtin.Mod builtin -> check_builtin_binop_integral env builtin.ty args kontinue
  | Builtin.Exp builtin -> check_builtin_binop_floating_point env builtin.ty args kontinue
  | Builtin.Promote builtin -> check_builtin_promote env builtin.sub builtin.sup args kontinue
  | Builtin.Concat builtin -> check_builtin_concat env builtin.ty args kontinue

and check_builtin_binop_args env ty lhs rhs kontinue =
  check_atom env lhs (fun inferred ->
    if Type.ty_equal ty inferred
    then
      check_atom env rhs (fun inferred ->
        if Type.ty_equal ty inferred
        then kontinue ty
        else mismatched_types inferred ty)
    else mismatched_types inferred ty)

and check_builtin_binop_integral env ty args kontinue = check_builtin_binop Type.ty_is_integral check_builtin_binop_args Builtin.not_integral env ty args kontinue
and check_builtin_binop_floating_point env ty args kontinue = check_builtin_binop Type.ty_is_floating_point check_builtin_binop_args Builtin.not_floating_point env ty args kontinue
and check_builtin_binop_numeric env ty args kontinue = check_builtin_binop Type.ty_is_numeric check_builtin_binop_args Builtin.not_numeric env ty args kontinue

and check_builtin_promote env sub sup args kontinue = match args with
  | arg :: [] ->
    check_atom env arg (fun inferred ->
      if Type.ty_equal inferred sub
      then kontinue sup
      else mismatched_types inferred sub)
  | _ -> invalid_arity 1 args

and check_builtin_concat env ty args kontinue = match ty with
  | Type.TyString -> check_builtin_concat_args env ty args kontinue
  | _ -> Builtin.unsupported_concat_type ty

and check_builtin_concat_args env ty args kontinue = match args with
  | [] -> kontinue ty
  | arg :: args ->
    check_atom env arg (fun inferred ->
      if Type.ty_equal inferred ty
      then check_builtin_concat_args env ty args kontinue
      else unsupported_concat_arg arg inferred ty)

(* Atoms *)

and check_atom env atom kontinue = match atom with
  | Ast.Bool _ -> kontinue Type.ty_bool
  | Ast.Int _ -> kontinue Type.ty_int
  | Ast.Long _ -> kontinue Type.ty_long
  | Ast.Float _ -> kontinue Type.ty_float
  | Ast.Double _ -> kontinue Type.ty_double
  | Ast.Rune _ -> kontinue Type.ty_rune
  | Ast.String _ -> kontinue Type.ty_string
  | Ast.Ident atom -> check_atom_ident env atom.id kontinue

and check_atom_ident env id kontinue =
  try
    Env.lookup id env
      |> kontinue
  with Not_found -> unbound_identifier id

(* Expressions *)

let check_expr env expr kontinue = match expr with
  | Ast.Builtin expr -> check_builtin env expr.fn expr.args kontinue
  | Ast.Atom expr -> check_atom env expr.atom kontinue

(* Patterns *)

let check_patt env patt ty kontinue = match patt with
  | Ast.PattGround -> kontinue env
  | Ast.PattVar patt -> Env.bind patt.id ty env kontinue

(* Bindings *)

let check_binding env binding kontinue = match binding with
  | Ast.Binding binding ->
    check_expr env binding.value (fun expr ->
      if Type.ty_equal binding.ty expr
      then check_patt env binding.patt expr kontinue
      else mismatched_types expr binding.ty)

(* Blocks *)

let rec check_block env block kontinue = match block with
  | Ast.Bind block -> check_block_bind env block.binding block.scope kontinue
  | Ast.Expr block -> check_block_expr env block.expr kontinue

and check_block_bind env binding scope kontinue =
  check_binding env binding (fun env ->
    check_block env scope kontinue)

and check_block_expr env expr kontinue =
  check_expr env expr (fun inferred ->
    inferred
      |> kontinue env)

(* Top-Level Expressions *)

let check_top env top kontinue = match top with
  | Ast.Let top -> check_binding env top.binding kontinue
