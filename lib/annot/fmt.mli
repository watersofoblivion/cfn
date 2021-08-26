
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
