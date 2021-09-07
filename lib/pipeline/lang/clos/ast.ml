(* Abstract Syntax *)

open Format

open Common

(* Syntax *)

type atom =
  | AtomBool of { value: bool }
  | AtomInt of { value: int32 }
  | AtomLong of { value: int64 }
  | AtomFloat of { value: float }
  | AtomDouble of { value: float }
  | AtomRune of { value: Uchar.t }
  | AtomString of { value: string; len: int }
  | AtomIdent of { id: Sym.t }

type expr =
  | ExprBuiltin of { fn: Builtin.builtin; args: atom list }
  | ExprAtom of { atom: atom }

type binding =
  | Binding of { patt: Patt.patt; ty: Type.ty; value: expr }

type block =
  | BlockLet of { binding: binding; scope: block }
  | BlockExpr of { expr: expr }

type top =
  | TopLet of { binding: binding }

(* Constructors *)

let atom_bool value = AtomBool { value }
let atom_int value = AtomInt { value }
let atom_long value = AtomLong { value }
let atom_float value = AtomFloat { value }
let atom_double value = AtomDouble { value }
let atom_rune value = AtomRune { value }
let atom_string value =
  let value = Utf8.normalize value in
  AtomString { value; len = Utf8.length value }
let atom_ident id = AtomIdent { id }

let expr_builtin fn args = ExprBuiltin { fn; args }
let expr_atom atom = ExprAtom { atom }

let binding patt ty value = Binding { patt; ty; value }

let block_let binding scope = BlockLet { binding; scope }
let block_expr expr = BlockExpr { expr }

let top_let binding = TopLet { binding }

(* Pretty Printing *)

(* Atoms *)

let pp_atom fmt = function
  | AtomBool atom -> fprintf fmt "%B" atom.value
  | AtomInt atom -> fprintf fmt "%ld" atom.value
  | AtomLong atom -> fprintf fmt "%Ld" atom.value
  | AtomFloat atom -> fprintf fmt "%g" atom.value
  | AtomDouble atom -> fprintf fmt "%g" atom.value
  | AtomRune atom ->
    atom.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | AtomString atom ->fprintf fmt "%S" atom.value
  | AtomIdent atom -> Sym.pp fmt atom.id

(* Expressions *)

let rec pp_expr fmt = function
  | ExprBuiltin expr -> pp_expr_builtin fmt expr.fn expr.args
  | ExprAtom expr -> pp_atom fmt expr.atom

and pp_expr_builtin fmt fn args =
  let pp_sep fmt _ = fprintf fmt ", " in
  fprintf fmt "%a(%a)" Builtin.pp_builtin fn (pp_print_list ~pp_sep pp_atom) args

(* Bindings *)

let pp_binding fmt = function
  | Binding binding ->
    fprintf fmt "%a: %a = %a" Patt.pp_patt binding.patt Type.pp_ty binding.ty pp_expr binding.value

(* Blocks *)

let rec pp_block fmt = function
  | BlockLet block -> pp_block_let fmt block.binding block.scope
  | BlockExpr block -> pp_expr fmt block.expr

and pp_block_let fmt binding scope =
  fprintf fmt "let %a = %a" pp_binding binding pp_block scope

(* Top-Level Expressions *)

let pp_top fmt = function
  | TopLet top ->
    fprintf fmt "let %a" pp_binding top.binding

(* Type Checking *)

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Type.ty * Type.ty
exception InvalidArity of int * int
exception UnsupportedConcatArg of atom * Type.ty * Type.ty

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

(* Atoms *)

let rec check_atom env atom kontinue = match atom with
  | AtomBool _ -> kontinue Type.ty_bool
  | AtomInt _ -> kontinue Type.ty_int
  | AtomLong _ -> kontinue Type.ty_long
  | AtomFloat _ -> kontinue Type.ty_float
  | AtomDouble _ -> kontinue Type.ty_double
  | AtomRune _ -> kontinue Type.ty_rune
  | AtomString _ -> kontinue Type.ty_string
  | AtomIdent atom -> check_atom_ident env atom.id kontinue

and check_atom_ident env id kontinue =
  try
    Env.lookup id env
      |> kontinue
  with Not_found -> unbound_identifier id

(* Expressions *)

let rec check_expr env expr kontinue = match expr with
  | ExprBuiltin expr -> check_expr_builtin env expr.fn expr.args kontinue
  | ExprAtom expr -> check_atom env expr.atom kontinue

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
    check_atom env arg (fun inferred ->
      if Type.ty_equal ty inferred
      then check_expr_builtin_fixed_args env tys res args kontinue
      else mismatched_types inferred ty)
  | _ ->
    Invalid_argument "Cannot happen"
      |> raise

and check_expr_builtin_var env ty res args kontinue = match args with
  | [] -> kontinue res
  | arg :: args ->
    check_atom env arg (fun inferred ->
      if Type.ty_equal ty inferred
      then check_expr_builtin_var env ty res args kontinue
      else mismatched_types inferred ty)

(* Bindings *)

let check_binding env binding kontinue = match binding with
  | Binding binding ->
    check_expr env binding.value (fun expr ->
      if Type.ty_equal binding.ty expr
      then Patt.check_patt env binding.patt expr kontinue
      else mismatched_types expr binding.ty)

(* Blocks *)

let rec check_block env block kontinue = match block with
  | BlockLet block -> check_block_let env block.binding block.scope kontinue
  | BlockExpr block -> check_block_expr env block.expr kontinue

and check_block_let env binding scope kontinue =
  check_binding env binding (fun env ->
    check_block env scope kontinue)

and check_block_expr env expr kontinue =
  check_expr env expr (fun inferred ->
    inferred
      |> kontinue env)

(* Top-Level Expressions *)

let check_top env top kontinue = match top with
  | TopLet top -> check_binding env top.binding kontinue
