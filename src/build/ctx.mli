(**
 * {1 Build Context}
 *
 * The build
 *)

type t
(** A build context *)

val empty : t
(** [ctx] returns a context with no values set. *)

(**
 * {2 Accessors}
 *
 * Functions to access the state of the context
 *)

val clang_exe : t -> string
(** [clang_exe ctx] returns the path to the Clang executable.  Raises
    {!Not_found} if the executable is not set. *)

val docker_exe : t -> string
(** [docker_exe ctx] returns the path to the Docker executable.  Raises
    {!Not_found} if the executable is not set. *)

val cfn_home : t -> string
(** [cfn_home ctx] returns the path to the CFN++ home directory.  Raises
    {!Not_found} if the directory is not set. *)

val project_root : t -> string
(** [project_root ctx] returns the path to the root of the current project.
    Raises {!Not_found} if the directory is not set. *)

(**
 * {2 Builders}
 *
 * Functions that update the state of the context
 *)

val with_clang : string -> t -> t
(** [with_clang path ctx] returns a copy of [ctx] with the Clang executable path
    set to [path].  Raises {!Invalid_argument} if the path does not point to an
    executable file. *)

val with_docker : string -> t -> t
(** [with_stdlib path ctx] return a copy of [ctx] with the Docker executable
    path set to [path].  Raises {!Invalid_argument} if the path does not point
    to an executable. *)

val with_cfn_home : string -> t -> t
(** [with_cfn_home path ctx] returns a copy of [ctx] with the CFN++ home
    directory set to [path].  Raises {!Invalid_argument} if the path does not
    point to a directory. *)

val with_project_root : string -> t -> t
(** [with_project_root path ctx] returns a copy of [ctx] with the project root
    directory set to [path].  Raises {!Invalid_argument} if the path does not
    point to a directory. *)

val from_env : t -> t
(** [from_env ctx] populates a context from the environment.  This is built
    on top of the builder functions and fails when they fail.

    A Clang executable named [clang] is searched for in the path.  Raises
    {!Invalid_argument} if one is not found.

    A Docker executable named [docker] is searched for in the path.  Raises
    {!Invalid_argument} if one is not found.

    The standard library path is assumed to be in the [${CFN_HOME}] environment
    variable.  Raises {!Failure} if the environment variable is unset.

    A file named "project.json" is searched for in the current directory or any
    parent directory.  The most deeply nested directory containing the file is
    assumed to be the project root.  Raises {!Failure} if the file is not
    found. *)

(**
 * {2 Path Helpers}
 *
 * Functions to construct paths that depend on the context.
 *)

val cfn_stdlib_dir : string
(** [cfn_stdlib_dir] is the name of the directory within the CFN++ home
   directory containing the standard library. *)

val cfn_stdlib : t -> string
(** [cfn_stdlib ctx] returns the path to the root of the CFN++ standary library.
   This path is [<cfn_home>/<cfn_stdlib_dir>].  Raises {!Not_found} if the
   CFN++ home directory is not set. *)

val cfn_shared_dir : string
(** [cfn_shared_dir] is the name of the directory within the CFN++ home
   directory containing shared resources. *)

val cfn_shared : t -> string
(** [cfn_shared ctx] returns the path to the directory containing CFN++ shared
   resources.  This path is [<cfn_home>/<cfn_shared_dir>].  Raises {!Not_found}
   if the CFN++ home directory is not set. *)

val build_dir : string
(** [build_dir] is the name of the build directory. *)

val build_path : t -> string
(** [build_path ctx] returns the path containing all of the sources and build
    artifacts for the current project.  This path is
    [<project_root>/<build_dir>]. *)

val src_dir : string
(** [src_dir] is the name of the directory within the build directory containing
    package sources. *)

val src_path : t -> string
(** [src_path ctx] returns the path containing CFN++ sources of all dependencies.
    his path is [<build_path>/<src_dir>]. *)

val lib_dir : string
(** [lib_dir] is the name of the directory within the build directory containing
    compiled build artifacts. *)

val lib_path : t -> string
(** [lib_path ctx] returns the path containing compiled build artifacts.  This
    path is [<build_path>/<lib_dir>]. *)
