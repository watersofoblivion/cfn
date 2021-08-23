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

val tops : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
(** [tops pp_top fmt file] pretty-prints the sequence of top-level expressions
    [tops] to the formatter [fmt] using the top-level pretty-printer [pp_top]. *)

val err_at : Loc.t -> formatter -> (formatter -> 'a) -> 'a
val compiler_bug : formatter -> (formatter -> 'a) -> 'a
