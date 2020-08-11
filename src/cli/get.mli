open Cmdliner

(**
 * {1 Get Command}
 *)

val impl : bool -> bool -> string list -> unit
(** [impl update pre_release project_paths] fetches all dependencies of the
    projects listed in [project_paths], or all dependencies of the current
    project if [project_paths] is empty.  Updates the dependencies to the
    latest released version if [update] is [true], and updates to the latest
    pre-release version if both [update] and [pre_release] are [true]. *)

val cmd : unit Term.t * Term.info
(** [cmd] is the {!Cmdliner} [get] command. *)
