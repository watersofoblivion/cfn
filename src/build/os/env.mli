(**
 * {1 Environment}
 *
 * Handling of environment variables
 *)

type var
(** An environment variable *)

val var : string -> var
(** [var name] constructs an environment variable named [name]. *)

val get : var -> string
(** [get var] gets the environment variable [var].  Raises {!Not_found} if the
    environment variable is not set. *)

val get_opt : var -> string option
(** [get_opt var] gets the environment variable [var].  Returns [Some value] if
    the variable is set, or [None] otherwise. *)

val set : var -> string -> unit
(** [set var value] sets the environment variable [var] to [value]. *)
