open Common

type t =
  | Constr of { loc: Loc.t; id: Sym.t }

(* Constructors *)

let constr loc id = Constr { loc; id }
