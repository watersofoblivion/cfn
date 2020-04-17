(**
 * {1 Pretty-Printing}
 *
 * Pretty printing of source files.
 *)

open Format

(** {2 Package Statement} *)

val package_stmt : formatter -> Ast.package_stmt -> unit
(** [package_stmt fmt stmt] pretty-prints [stmt] to [fmt]. *)

(** {2 Imports} *)

val from_clause : formatter -> Ast.from_clause -> unit
(** [from_clause fmt from] pretty-prints [from] to [fmt]. *)

val package_alias : formatter -> Ast.package_alias -> unit
(** [package_alias fmt alias] pretty-prints [alias] to [fmt]. *)

val package_clause : formatter -> Ast.package_clause -> unit
(** [package_clause fmt pkg] pretty-prints [pkg] to [fmt]. *)

val import_clause : formatter -> Ast.import_clause -> unit
(** [import_clause fmt import] pretty-prints [import] to [fmt]. *)

val import_stmt : formatter -> Ast.import_stmt -> unit
(** [import_stmt fmt stmt] pretty-prints [stmt] to [fmt]. *)

(** {2 Files} *)

val file : formatter -> Ast.file -> unit
(** [file fmt f] pretty-prints [f] to [fmt]. *)
