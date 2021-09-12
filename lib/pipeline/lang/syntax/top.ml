open Format

open Common

(* Syntax *)

type top =
  | TopLet of { loc: Loc.t; binding: Expr.binding }
  | TopVal of { loc: Loc.t; binding: Expr.binding }

(* Constructors *)

let top_let loc binding = TopLet { loc; binding }
let top_val loc binding = TopVal { loc; binding }

(* Locations *)

let loc_top = function
  | TopLet top -> top.loc
  | TopVal top -> top.loc

(* Pretty Printing *)

(* Top-Level Expressions *)

let pp_top fmt = function
  | TopLet top -> fprintf fmt "let %a" Expr.pp_binding top.binding
  | TopVal top -> fprintf fmt "val %a" Expr.pp_binding top.binding
