(* Type Checking *)

open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Ast.expr * Type.ty * Type.ty
exception InvalidArity of int * int
exception UnsupportedConcatArg of Ast.expr * Type.ty * Type.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types expr inferred annotated =
  MismatchedTypes (expr, inferred, annotated)
    |> raise

let invalid_arity expected lst =
  let actual = List.length lst in
  InvalidArity (expected, actual)
    |> raise

let unsupported_concat_arg arg inferred expected =
  UnsupportedConcatArg (arg, inferred, expected)
    |> raise

(* Patterns *)

let check_patt env patt ty kontinue = match patt with
  | Ast.PattGround -> kontinue env
  | Ast.PattVar patt -> Env.bind patt.id ty env kontinue

(* Builtins *)

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
  check_expr env lhs (fun inferred ->
    if Type.ty_equal ty inferred
    then
      check_expr env rhs (fun inferred ->
        if Type.ty_equal ty inferred
        then kontinue ty
        else mismatched_types rhs inferred ty)
    else mismatched_types lhs inferred ty)

and check_builtin_binop_integral env ty args kontinue = check_builtin_binop Type.ty_is_integral check_builtin_binop_args Builtin.not_integral env ty args kontinue
and check_builtin_binop_floating_point env ty args kontinue = check_builtin_binop Type.ty_is_floating_point check_builtin_binop_args Builtin.not_floating_point env ty args kontinue
and check_builtin_binop_numeric env ty args kontinue = check_builtin_binop Type.ty_is_numeric check_builtin_binop_args Builtin.not_numeric env ty args kontinue

and check_builtin_promote env sub sup args kontinue = match args with
  | arg :: [] ->
    check_expr env arg (fun inferred ->
      if Type.ty_equal inferred sub
      then kontinue sup
      else mismatched_types arg inferred sub)
  | _ -> invalid_arity 1 args

and check_builtin_concat env ty args kontinue = match ty with
  | Type.TyString -> check_builtin_concat_args env ty args kontinue
  | _ -> Builtin.unsupported_concat_type ty

and check_builtin_concat_args env ty args kontinue = match args with
  | [] -> kontinue ty
  | arg :: args ->
    check_expr env arg (fun inferred ->
      if Type.ty_equal inferred ty
      then check_builtin_concat_args env ty args kontinue
      else unsupported_concat_arg arg inferred ty)

(* Expressions *)

and check_expr env expr kontinue = match expr with
  | Ast.Bool _ -> kontinue Type.ty_bool
  | Ast.Int _ -> kontinue Type.ty_int
  | Ast.Long _ -> kontinue Type.ty_long
  | Ast.Float _ -> kontinue Type.ty_float
  | Ast.Double _ -> kontinue Type.ty_double
  | Ast.Rune _ -> kontinue Type.ty_rune
  | Ast.String _ -> kontinue Type.ty_string
  | Ast.Ident expr -> check_expr_ident env expr.id kontinue
  | Ast.Builtin expr -> check_builtin env expr.fn expr.args kontinue

and check_expr_ident env id kontinue =
  try
    Env.lookup id env
      |> kontinue
  with Not_found -> unbound_identifier id

(* Bindings *)

let check_binding env binding kontinue = match binding with
  | Ast.Binding binding ->
    check_expr env binding.value (fun inferred ->
      if Type.ty_equal binding.ty inferred
      then check_patt env binding.patt inferred kontinue
      else mismatched_types binding.value inferred binding.ty)

(* Top-Level Expressions *)

let check_top env top kontinue = match top with
  | Ast.Let top -> check_binding env top.binding kontinue
