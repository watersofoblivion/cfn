
(**
 * {1 Pretty Printing}
 *)

open Format

(** {2 Types} *)

val ty : formatter -> Type.t -> unit
(** [ty fmt ty] pretty-prints [ty] to [fmt]. *)

(** {2 Expressions} *)

val expr : formatter -> Ast.expr -> unit
(** [expr fmt expr] pretty-prints [expr] to [fmt]. *)

(** {2 Patterns} *)

val patt : formatter -> Ast.patt -> unit
(** [patt fmt patt] pretty-prints [patt] to [fmt]. *)

(** {2 Bindings} *)

val binding : formatter -> Ast.binding -> unit
(** [binding fmt binding] pretty-prints [binding] to [fmt]. *)

(** {2 Top-Level Expressions} *)

val top : formatter -> Ast.top -> unit
(** [top fmt top] pretty-prints [top] to [fmt]. *)
