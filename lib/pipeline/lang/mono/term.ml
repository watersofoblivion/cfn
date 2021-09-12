(* Terms *)

open Format

(* Syntax *)

type binding =
  | Binding of { patt: Patt.patt; ty: Type.ty; value: Expr.expr }

type term =
  | TermLet of { binding: binding; scope: term }
  | TermExpr of { expr: Expr.expr }

(* Constructors *)

let binding patt ty value = Binding { patt; ty; value }

let term_let binding scope = TermLet { binding; scope }
let term_expr expr = TermExpr { expr }

(* Pretty Printing *)

let pp_binding fmt = function
  | Binding binding ->
    fprintf fmt "%a: %a = %a"
      Patt.pp_patt binding.patt
      Type.pp_ty binding.ty
      Expr.pp_expr binding.value

let rec pp_term fmt = function
  | TermLet term -> pp_term_let fmt term.binding term.scope
  | TermExpr term -> Expr.pp_expr fmt term.expr

and pp_term_let fmt binding scope =
  fprintf fmt "let %a in %a" pp_binding binding pp_term scope

(* Type Checking *)

let check_binding env binding kontinue = match binding with
  | Binding binding ->
    Expr.check_expr env binding.value (fun expr ->
      if Type.ty_equal binding.ty expr
      then Patt.check_patt env binding.patt expr kontinue
      else Check.mismatched_types expr binding.ty)

let rec check_term env term kontinue = match term with
  | TermLet term -> check_term_let env term.binding term.scope kontinue
  | TermExpr term -> check_term_expr env term.expr kontinue

and check_term_let env binding scope kontinue =
  check_binding env binding (fun env ->
    check_term env scope kontinue)

and check_term_expr env expr kontinue =
  Expr.check_expr env expr (fun inferred ->
    inferred
      |> kontinue env)
