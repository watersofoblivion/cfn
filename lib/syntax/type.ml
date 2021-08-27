open Common

type t =
  | Constr of { loc: Loc.t; id: Sym.t }

(* Constructors *)

let constr loc id = Constr { loc; id }

(* Operations *)

let equal ty ty' = match (ty, ty') with
  | Constr ty, Constr ty' -> Sym.equal ty.id ty'.id

let loc_ty = function
  | Constr ty -> ty.loc
