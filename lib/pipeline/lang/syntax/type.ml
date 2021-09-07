(* Types *)

open Common

(* Syntax *)

type ty =
  | TyConstr of { loc: Loc.t; id: Sym.t }

(* Constructors *)

let ty_constr loc id = TyConstr { loc; id }

(* Operations *)

let ty_equal ty ty' = match (ty, ty') with
  | TyConstr ty, TyConstr ty' -> Sym.equal ty.id ty'.id

let loc_ty = function
  | TyConstr ty -> ty.loc

(* Pretty Printing *)

let pp_ty fmt = function
  | TyConstr constr -> Sym.pp_id fmt constr.id
