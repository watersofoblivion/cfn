open Cmdliner

(**
 {1 Format Command}
 *)

val cmd : unit Term.t * Cmd.info
(**
  The [fmt] command.

  @since 1.0
*)
