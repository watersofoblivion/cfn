(**
 {1 Docker}
 *)

(**
 {2 Command}
 *)

val docker : string list -> (bytes -> 'a) -> 'a
(** [docker args handler] runs the "docker" command with command-line arguments
    [args].  On success, returns the result of passing standard output to
    [handler].  On failure (non-zero exit status), raises {!Os.NonZero}. *)

(**
 {2 Images}
 *)

type i
(** A docker image *)

val image : string -> string -> i
(** [image name tag] constructs a reference to an image with id [name:tag].
    Does not build the image with Docker. *)

val img_id : i -> string
(** [img_id img] returns the [name:tag] pair of [img]. *)

val img_name : i -> string
(** [img_name img] returns the name of [img]. *)

val img_tag : i -> string
(** [img_tag img] returns the tag of [img]. *)

val build : string -> string -> string -> i
(** [build dir name tag] builds the image in [dir] and names it [name:tag]. *)

(**
 {2 Containers}
 *)

type c
(** A docker container *)

(**
 {2 Operations}
 *)

val run : i -> c
(** [run img] runs a Docker image. *)
