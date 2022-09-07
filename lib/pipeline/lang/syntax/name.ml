(* Source Files *)

open Format

open Common

(* Syntax *)

type name = Name of { loc: Loc.t; id: Sym.t }

(* Constructors *)

let name loc id = Name { loc; id }

(* Location *)

let loc_name = function
  | Name name -> name.loc

(* Pretty Printing *)

let pp_name fmt = function
  | Name name -> fprintf fmt "%a" Sym.pp_id name.id
