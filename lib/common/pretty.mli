open Format

(**
 * {1 Pretty-Printing Helpers}
 *)

val ground : formatter -> unit
(** [ground fmt] pretty-print ground ([_]) to the formatter [fmt]. *)

val bool : formatter -> bool -> unit
(** [bool fmt b] pretty-prints the boolean value [b] to the formatter [fmt]. *)

val int : formatter -> int -> unit
(** [int fmt i] pretty-prints the integer value [i] to the formatter [fmt]. *)
