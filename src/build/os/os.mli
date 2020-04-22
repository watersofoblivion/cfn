(**
 * {1 Operating System}
 *
 * Abstraction layer over the host operating system.
 *)

val which : string -> string
val find_in_path : string -> string -> string -> string

(**
 * {2 Filesystem Helpers}
 *
 * The build process is filesystem heavy, so a few helpers are provided to clean
 * things up.
 *)

val mkpath : string list -> string
(** [mkpath segments] combines [segments] into a path.  If the first segment is
    not an absolute path, it is assumed to be relative to the current working
    directory as returned by {!Sys.getcwd}.  Paths starting with special
    prefixes refering to the current or parent directory ([.] or [..],
    respectively) are handled properly. *)

val mkdir_p : string -> string
(** [mkdir_p path] creates a directory and any necessary parent directories.  If
    [path] is not an absolute path, it is assumed to be relative to the current
    working directory as returned by {!Sys.getcwd}.  The created directory is
    returned. *)

val in_dir : string -> ('a -> 'b) -> 'a -> 'b
(** [in_dir path fn x] temporarily changes directory to [path] and applies [fn]
    to [x]. *)

val rm_rf : string -> unit
(** [rm_rf path] recursively empties and removes a directory.  If [path] is not
    an absolute path, it is assumed to be relative to the current working
    directory as returned by {!Sys.getcwd}. *)

val in_temp_dir : ('a -> 'b) -> 'a -> 'b
(** [in_temp_dir fn x] creates a temporary directory and uses {!in_dir} to
    change to it and apply [fn] to [x].  The directory is removed with {!rm_rf}
    once the function returns. *)
