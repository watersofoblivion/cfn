(* Top-Level Expressions *)

open Format

(* Syntax *)

type top =
  | TopLet of { binding: Term.binding }

(* Constructors *)

let top_let binding = TopLet { binding }

(* Pretty Printing *)

let pp_top fmt = function
  | TopLet top -> fprintf fmt "let %a" Term.pp_binding top.binding

(* Type Checking *)

let check_top env top kontinue = match top with
  | TopLet top -> Term.check_binding env top.binding kontinue
