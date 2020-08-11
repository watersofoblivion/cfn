open Cmdliner

(**
 * {1 Init Command}
 *)

val impl : string -> string -> unit
(** [impl id id] initializes a new project [id] in [dir]. *)

val cmd : unit Term.t * Term.info
(** [cmd] is the {!Cmdliner} [init] command. *)
