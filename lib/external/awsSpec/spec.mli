type t = private Service.t list

val of_dir : string -> t
(**
  Parse all of the services in a directory.

  @param dir The directory containing the services
  @return The services in the directory
  @since 1.0
*)
