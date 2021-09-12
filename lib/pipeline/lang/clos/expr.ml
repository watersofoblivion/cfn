(* Expressions *)

open Format

(* Syntax *)

type expr =
  | ExprBuiltin of { fn: Builtin.builtin; args: Atom.atom list }
  | ExprAtom of { atom: Atom.atom }

(* Constructors *)

let expr_builtin fn args = ExprBuiltin { fn; args }
let expr_atom atom = ExprAtom { atom }

(* Pretty Printing *)

let rec pp_expr fmt = function
  | ExprBuiltin expr -> pp_expr_builtin fmt expr.fn expr.args
  | ExprAtom expr -> Atom.pp_atom fmt expr.atom

and pp_expr_builtin fmt fn args =
  let pp_sep fmt _ = fprintf fmt " " in
  fprintf fmt "%a %a" Builtin.pp_builtin fn (pp_print_list ~pp_sep Atom.pp_atom) args

(* Type Checking *)

let rec check_expr env expr kontinue = match expr with
  | ExprBuiltin expr -> check_expr_builtin env expr.fn expr.args kontinue
  | ExprAtom expr -> Atom.check_atom env expr.atom kontinue

and check_expr_builtin env fn args kontinue =
  Builtin.check_builtin env fn (function
    | Builtin.ArityFixed arity -> check_expr_builtin_fixed env arity.arity arity.args arity.res args kontinue
    | Builtin.ArityVar arity -> check_expr_builtin_var env arity.args arity.res args kontinue)

and check_expr_builtin_fixed env arity tys res args kontinue =
  let actual = List.length args in
  if arity = actual
  then check_expr_builtin_fixed_args env tys res args kontinue
  else Check.invalid_arity arity args

and check_expr_builtin_fixed_args env tys res args kontinue = match (tys, args) with
  | [], [] -> kontinue res
  | ty :: tys, arg :: args ->
    Atom.check_atom env arg (fun inferred ->
      if Type.ty_equal ty inferred
      then check_expr_builtin_fixed_args env tys res args kontinue
      else Check.mismatched_types inferred ty)
  | _ ->
    Invalid_argument "Cannot happen"
      |> raise

and check_expr_builtin_var env ty res args kontinue = match args with
  | [] -> kontinue res
  | arg :: args ->
    Atom.check_atom env arg (fun inferred ->
      if Type.ty_equal ty inferred
      then check_expr_builtin_var env ty res args kontinue
      else Check.mismatched_types inferred ty)
