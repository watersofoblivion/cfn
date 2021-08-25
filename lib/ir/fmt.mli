
(**
 * {1 Pretty Printing}
 *)

open Format

(** {2 Atoms} *)

val atom : formatter -> Ast.atom -> unit
(** [atom fmt atom] pretty-prints [atom] to [fmt]. *)

(** {2 Expressions} *)

val expr : formatter -> Ast.expr -> unit
(** [expr fmt expr] pretty-prints [expr] to [fmt]. *)

(** {2 Blocks} *)

val block : formatter -> Ast.block -> unit
(** [block fmt block] pretty-prints [block] to [fmt]. *)
