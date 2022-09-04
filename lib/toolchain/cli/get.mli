open Cmdliner

(**
 {1 Get Command}
 *)

val impl : bool -> bool -> string list -> unit
(**
  Fetch all dependencies of a set of projects or of the current project.

  @param update Update the dependencies to the latest released version
  @param pre_release Allow updates to pre-release versions
  @param project_paths The projects to update
  @since 1.0
*)

val cmd : unit Term.t * Cmd.info
(**
  The [get] command.

  @since 1.0
*)
