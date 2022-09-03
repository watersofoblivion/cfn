open Format

(**
 * {1 Pretty-Printing Helpers}
 *)

val ground : formatter -> unit
(**
  Pretty-print ground ([_]) to a formatter.

  @param fmt The formatter to print to
  @since 1.0
*)

val bool : formatter -> bool -> unit
(**
  Pretty-print a boolean value to a formatter.

  @param fmt The formatter to print to
  @param b The value to print
  @since 1.0
*)

val int : formatter -> int -> unit
(**
  Pretty-print an integer value to a formatter.

  @param fmt The formatter to print to
  @param i The value to print
  @since 1.0
*)
