open Cmdliner

(**
 {1 Init Command}
 *)

val impl : string -> string -> unit
(**
  Initialize a new project.

  @param id The name of the project
  @param dir The directory to initialize the project in
  @since 1.0
*)

val cmd : unit Term.t * Cmd.info
(**
  The [init] command.

  @since 1.0
*)
