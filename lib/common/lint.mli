open Format

(**
 * {1 Linting}
 *)

type t
(** A piece of lint *)

(**
 * {2 Constructors}
 *)

(**
 * {2 Operations}
 *)

val pp : t -> formatter -> unit
(** [pp lint formatter] pretty-prints the piece of lint to the formatter [fmt]. *)

(**
 * {2 Errors}
 *)

type res
(** A set of linting errors *)

val empty : res
(** [empty] constructs an empty set of linting errors *)

val merge : res -> res -> res
(** [merge errs errs'] merges the sets of linting errors [errs] with [errs']. *)
