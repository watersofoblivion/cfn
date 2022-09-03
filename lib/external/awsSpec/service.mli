(**
 {1 Services}
 *)

type t = private {
  name:     string;
  versions: (string * Version.t) list
}

val of_dir : string -> t
(**
  Parse all of the versions of a service from a directory.

  @param dir The directory containing the version of a service
  @return The versions of a service
  @since 1.0
*)
