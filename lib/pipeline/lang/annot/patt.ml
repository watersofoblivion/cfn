(* Patterns *)

open Common

(* Syntax *)

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

(* Constructors *)

let patt_ground = PattGround
let patt_var id = PattVar { id }

(* Pretty Printing *)

let pp_patt fmt = function
  | PattGround -> Pretty.ground fmt
  | PattVar patt -> Sym.pp fmt patt.id

(* Type Checking *)

let check_patt env patt ty kontinue = match patt with
  | PattGround -> kontinue env
  | PattVar patt -> Env.bind patt.id ty env kontinue
