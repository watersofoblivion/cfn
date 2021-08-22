type t = private Service.t list

val of_dir : string -> t
(** [of_dir dir] parses all of the services in a directory. *)
