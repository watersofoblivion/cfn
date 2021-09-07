(* Patterns *)

open Common

(* Syntax *)

type patt =
  | PattGround of { loc: Loc.t }
  | PattVar of { loc: Loc.t; id: Sym.t }

(* Constructors *)

let patt_ground loc = PattGround { loc }
let patt_var loc id = PattVar { loc; id }

(* Location *)

let loc_patt = function
  | PattGround patt -> patt.loc
  | PattVar patt -> patt.loc

(* Pretty Printing *)

let pp_patt fmt = function
  | PattGround _ -> Pretty.ground fmt
  | PattVar patt -> Sym.pp_id fmt patt.id
