(**
 * {1 Build}
 *
 * Contains all of the infrastructure for building CFN++ projects.
 *)

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

(**
 * {2 Build Context}
 *
 * The build
 *)

(** A build context *)
type ctx = private {
  clang:  string; (** The path to the Clang executable *)
  stdlib: string; (** The path to the standard library *)
  home:   string; (** The user's home directory *)
  root:   string  (** The root of the current project. *)
}

val empty_ctx : ctx
(** [empty_ctx] returns a context with no values set. *)

val ctx_with_clang : string -> ctx -> ctx
(** [ctx_with_clang path ctx] returns a copy of [ctx] with the Clang field set
    to [path].  Raises {!Failure} if the path does not point to an executable
    file. *)

val ctx_with_stdlib : string -> ctx -> ctx
(** [ctx_with_stdlib path ctx] return a copy of [ctx] with the standard library
    field set to [path].  Raises {!Failure} if the path does not point to a
    directory. *)

val ctx_with_home : string -> ctx -> ctx
(** [ctx_with_home path ctx] returns a copy of [ctx] with the home directory set
    to [path].  Raises {!Failure} if the path does not point to a directory. *)

val ctx_with_root : string -> ctx -> ctx
(** [ctx_with_root path ctx] returns a copy of [ctx] with the project root
    directory set to [path].  Raises {!Failure} if the path does not point to a
    directory. *)

val ctx_from_env : ctx -> ctx
(** [ctx_from env ctx] populates a context from the environment.  This is built
    on top of the [ctx_with_...] functions and fails when they fail.

    A Clang executable named [clang] is searched for in the path.  Raises
    {!Failure} if one is not found.

    The standard library path is assumed to be in the [${CFNROOT}] environment
    variable.  Raises {!Failure} if the environment variable is unset.

    The home directory is assumed to be in the [${HOME}] environment variable.

    A file named "project.json" is searched for in the current directory or any
    parent directory.  The most deeply nested directory containing the file is
    assumed to be the project root.  Raises {!Failure} if the file is not
    found. *)
