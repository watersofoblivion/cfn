(* Imports *)

open Format

open Common

(* Syntax *)

type pkg = Package of { name: Sym.t }

(* Constructors *)

let pkg name = Package { name }

(* Pretty-Printing *)

let pp_pkg fmt = function
  | Package pkg -> fprintf fmt "package %a" Sym.pp pkg.name
