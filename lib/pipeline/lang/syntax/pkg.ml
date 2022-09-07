(* Source Files *)

open Format

open Common

(* Syntax *)

type pkg = Package of { loc: Loc.t; id: Name.name }

(* Constructors *)

let pkg loc id = Package { loc; id }

(* Location *)

let loc_pkg = function
  | Package pkg -> pkg.loc

(* Pretty-Printing *)

let pp_pkg fmt = function
  | Package pkg -> fprintf fmt "package %a" Name.pp_name pkg.id
