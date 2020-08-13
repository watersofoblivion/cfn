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
