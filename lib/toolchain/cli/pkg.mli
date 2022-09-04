open Cmdliner

(**
 {1 Package Command}
 *)

val cmd : unit Term.t * Cmd.info
(**
  The [pkg] command.

  @since 1.0
*)
