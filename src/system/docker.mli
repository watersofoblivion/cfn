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

val build : string -> string -> string -> i
(** [build dir name tag] builds the image in [dir] and names it [name:tag]. *)

val img_id : i -> string
(** [img_id img] returns the [name:tag] pair of [img]. *)

val img_name : i -> string
(** [img_name img] returns the name of [img]. *)

val img_tag : i -> string
(** [img_tag img] returns the tag of [img]. *)

(**
 {2 Operations}
 *)

type mount
(** A mount point for a container *)

val local_dir : mount -> string
(** [local_dir m] returns the local directory of [m]. *)

val container_dir : mount -> string
(** [container_dir m] returns the container directory of [m]. *)

val dir_mount : string -> string -> mount
(** [dir_mount local_dir container_dir] creates a mount from [local_dir] on the
    local machine to [container_dir] on the container.  Raises
    {!Invalid_argument} if either [local_dir] or [container_dir] are not
    absolute paths. *)

type mounts
(** A set of mounts in a container *)

val mounts : mounts
(** [mounts] is an empty set of mounts. *)

val add_mount : mount -> mounts -> mounts
(** [add_mount mount mounts] adds a mount to the set [mounts]. *)

val run_in : string -> string list -> ?mounts:mounts -> i -> (bytes -> 'a) -> 'a
(** [run_in cmd args ?mounts img handler] runs a [cmd] with arguments [args] in
    a Docker container using the image [img] with [mounts] attached.  On
    success, passes the standard output of the command to [handler].  Otherwise,
    raises {!Os.NonZero}. *)
