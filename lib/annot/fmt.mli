
(**
 * {1 Pretty Printing}
 *)

open Format

(** {2 Expressions} *)

val expr : formatter -> Ast.expr -> unit
(** [expr fmt expr] pretty-prints [expr] to [fmt]. *)
