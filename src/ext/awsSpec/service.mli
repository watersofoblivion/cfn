(**
 * {1 Services}
 *)

type t = private {
  name:     string;
  versions: (string * Version.t) list
}

val of_dir : string -> t
(** [of_dir dir] parses all of the versions of a service from a directory. *)
