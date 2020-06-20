(**
 * {1 Build Context}
 *
 * The build
 *)

(** A build context *)
type t = private {
  clang:  string; (** The path to the Clang executable *)
  stdlib: string; (** The path to the standard library *)
  home:   string; (** The user's home directory *)
  root:   string  (** The root of the current project. *)
}

val ctx : t
(** [ctx] returns a context with no values set. *)

(**
 * {2 Builders}
 *)

val with_clang : string -> t -> t
(** [with_clang path ctx] returns a copy of [ctx] with the Clang field set
    to [path].  Raises {!Failure} if the path does not point to an executable
    file. *)

val with_stdlib : string -> t -> t
(** [with_stdlib path ctx] return a copy of [ctx] with the standard library
    field set to [path].  Raises {!Failure} if the path does not point to a
    directory. *)

val with_home : string -> t -> t
(** [with_home path ctx] returns a copy of [ctx] with the home directory set
    to [path].  Raises {!Failure} if the path does not point to a directory. *)

val with_root : string -> t -> t
(** [with_root path ctx] returns a copy of [ctx] with the project root
    directory set to [path].  Raises {!Failure} if the path does not point to a
    directory. *)

val from_env : t -> t
(** [from_env ctx] populates a context from the environment.  This is built
    on top of the builder functions and fails when they fail.

    A Clang executable named [clang] is searched for in the path.  Raises
    {!Failure} if one is not found.

    The standard library path is assumed to be in the [${CFNROOT}] environment
    variable.  Raises {!Failure} if the environment variable is unset.

    The home directory is assumed to be in the [${HOME}] environment variable.

    A file named "project.json" is searched for in the current directory or any
    parent directory.  The most deeply nested directory containing the file is
    assumed to be the project root.  Raises {!Failure} if the file is not
    found. *)

(**
 * {2 Path Helpers}
 *
 * Functions to construct paths to the various inputs and outputs of the build
 * process.  All paths are relative to the root of the project and
 * subdirectories of {!build_dir}.
 *)

val build_dir : t -> string
(** [build_dir ctx] returns the directory containing all of the build artifacts
    for the current project.  This path is [<project_root>/.cfn++]. *)

val src_dir : t -> Path.t -> string
(** [src_dir ctx path] returns the directory containing the source files of
    [path].  This path is [<build_dir>/src/<path>]. *)

val lib_dir : t -> Path.t -> string
(** [lib_dir ctx path] returns the directory containing the compiled
    intermediary files of [path].  This path is
    [<build_dir>/lib/<path>]. *)

val pkg_dir : t -> string
(** [pkg_dir ctx] returns the directoy containing the compiled output package of
    the current project.  This path is [<build_dir>/pkg]. *)
