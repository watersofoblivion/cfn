(* Abstract Syntax *)

open Format

open Common

(* Syntax *)

type expr =
  | ExprBool of { value: bool }
  | ExprInt of { value: int32 }
  | ExprLong of { value: int64 }
  | ExprFloat of { value: float }
  | ExprDouble of { value: float }
  | ExprRune of { value: Uchar.t }
  | ExprString of { value: string; len: int }
  | ExprIdent of { id: Sym.t }
  | ExprBuiltin of { fn: Builtin.builtin; args: expr list }
  | ExprLet of { binding: binding; scope: expr }

and binding =
  | Binding of { patt: Patt.patt; ty: Type.ty; value: expr }

type top =
  | TopLet of { binding: binding }

(* Constructors *)

let expr_bool value = ExprBool { value }
let expr_int value = ExprInt { value }
let expr_long value = ExprLong { value }
let expr_float value = ExprFloat { value }
let expr_double value = ExprDouble { value }
let expr_rune value = ExprRune { value }
let expr_string value =
  let value = Utf8.normalize value in
  ExprString { value; len = Utf8.length value }
let expr_ident id = ExprIdent { id }
let expr_builtin fn args = ExprBuiltin { fn; args }
let expr_let binding scope = ExprLet { binding; scope }

let binding patt ty value = Binding { patt; ty; value }

let top_let binding = TopLet { binding }

(* Pretty Printing *)

(* Expressions *)

let rec pp_expr fmt = function
  | ExprBool expr -> fprintf fmt "%B" expr.value
  | ExprInt expr -> fprintf fmt "%ld" expr.value
  | ExprLong expr -> fprintf fmt "%Ld" expr.value
  | ExprFloat expr -> fprintf fmt "%g" expr.value
  | ExprDouble expr -> fprintf fmt "%g" expr.value
  | ExprRune expr -> pp_expr_rune fmt expr.value
  | ExprString expr -> fprintf fmt "%S" expr.value
  | ExprIdent expr -> Sym.pp fmt expr.id
  | ExprBuiltin expr -> pp_expr_builtin fmt expr.fn expr.args
  | ExprLet expr -> pp_expr_let fmt expr.binding expr.scope

and pp_expr_rune fmt r =
  if r = Utf8.single_quote
  then fprintf fmt "'\\''"
  else
    r
      |> Utf8.to_string
      |> fprintf fmt "'%s'"

and pp_expr_builtin fmt fn args =
  let pp_sep fmt _ = fprintf fmt ",@ " in
  fprintf fmt "%a(%a)" Builtin.pp_builtin fn (pp_print_list ~pp_sep pp_expr) args

and pp_expr_let fmt binding scope =
  fprintf fmt "let %a in %a" pp_binding binding pp_expr scope

(* Bindings *)

and pp_binding fmt = function
  | Binding binding ->
    fprintf fmt "%a: %a = %a" Patt.pp_patt binding.patt Type.pp_ty binding.ty pp_expr binding.value

(* Top-Level Expressions *)

let pp_top fmt = function
  | TopLet top ->
    fprintf fmt "let %a" pp_binding top.binding

(* Type Checking *)

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of expr * Type.ty * Type.ty
exception InvalidArity of int * int
exception UnsupportedConcatArg of expr * Type.ty * Type.ty

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

(* Expressions *)

let rec check_expr env expr kontinue = match expr with
  | ExprBool _ -> kontinue Type.ty_bool
  | ExprInt _ -> kontinue Type.ty_int
  | ExprLong _ -> kontinue Type.ty_long
  | ExprFloat _ -> kontinue Type.ty_float
  | ExprDouble _ -> kontinue Type.ty_double
  | ExprRune _ -> kontinue Type.ty_rune
  | ExprString _ -> kontinue Type.ty_string
  | ExprIdent expr -> check_expr_ident env expr.id kontinue
  | ExprBuiltin expr -> check_expr_builtin env expr.fn expr.args kontinue
  | ExprLet expr -> check_expr_let env expr.binding expr.scope kontinue

and check_expr_ident env id kontinue =
  try
    Env.lookup id env
      |> kontinue
  with Not_found -> unbound_identifier id

and check_expr_builtin env fn args kontinue =
  Builtin.check_builtin env fn (function
    | Builtin.ArityFixed arity -> check_expr_builtin_fixed env arity.arity arity.args arity.res args kontinue
    | Builtin.ArityVar arity -> check_expr_builtin_var env arity.args arity.res args kontinue)

and check_expr_builtin_fixed env arity tys res args kontinue =
  let actual = List.length args in
  if arity = actual
  then check_expr_builtin_fixed_args env tys res args kontinue
  else invalid_arity arity args

and check_expr_builtin_fixed_args env tys res args kontinue = match (tys, args) with
  | [], [] -> kontinue res
  | ty :: tys, arg :: args ->
    check_expr env arg (fun inferred ->
      if Type.ty_equal ty inferred
      then check_expr_builtin_fixed_args env tys res args kontinue
      else mismatched_types arg inferred ty)
  | _ ->
    Invalid_argument "Cannot happen"
      |> raise

and check_expr_builtin_var env ty res args kontinue = match args with
  | [] -> kontinue res
  | arg :: args ->
    check_expr env arg (fun inferred ->
      if Type.ty_equal ty inferred
      then check_expr_builtin_var env ty res args kontinue
      else mismatched_types arg inferred ty)

and check_expr_let env binding scope kontinue =
  check_binding env binding (fun env ->
    check_expr env scope kontinue)

(* Bindings *)

and check_binding env binding kontinue = match binding with
  | Binding binding ->
    check_expr env binding.value (fun inferred ->
      if Type.ty_equal binding.ty inferred
      then Patt.check_patt env binding.patt inferred kontinue
      else mismatched_types binding.value inferred binding.ty)

(* Top-Level Expressions *)

let check_top env top kontinue = match top with
  | TopLet top -> check_binding env top.binding kontinue
