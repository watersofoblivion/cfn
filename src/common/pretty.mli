open Format

(**
 * {1 Pretty-Printing Helpers}
 *)

val ground : formatter -> unit
(** [ground fmt] pretty-print ground ([_]) to the formatter [fmt]. *)

val bool : bool -> formatter -> unit
(** [bool b fmt] pretty-prints the boolean value [b] to the formatter [fmt]. *)

val int : int -> formatter -> unit
(** [int i fmt] pretty-prints the integer value [i] to the formatter [fmt]. *)

val tops : ('a -> formatter -> unit) -> 'a list -> formatter -> unit
(** [tops pp_top file fmt] pretty-prints the sequence of top-level expressions
    [tops] to the formatter [fmt] using the top-level pretty-printer [pp_top]. *)

val err_at : Loc.t -> formatter -> (formatter -> 'a) -> 'a
val compiler_bug : formatter -> (formatter -> 'a) -> 'a
