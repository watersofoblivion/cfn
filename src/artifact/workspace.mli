(* open Syntax

(**
 * {1 Workspace}
 *
 * The build
 *)

(**
 * {2 Types}
 *)

type t
(** A workspace. *)

val current : unit -> t
(** [current _] returns the workspace for the current project.  Values set from
    the environment. *)

val create : Path.id -> string -> t
(** [create id dir] creates a new workspace for project [id] in directory [dir].
    Writes out the project file and lockfile, initializes a Git repository, and
    performs an initial commit.  Raises {!Invalid_argument} if [dir] is not an
    absolute path or already exists. *)

val get : Path.project list -> ?update:bool -> ?pre_release:bool -> t -> t
(** [update_all projs ?update ?pre_release ws] recursively fetches [projs] and
    all of their dependencies in the cache in [ws], updating dependecies as
    required.  If [projs] is empty, the current project in [ws] us fetched.  If
    [update] is [true] (default [false]), all projects and and dependencies are
    updated to their latest released versions. Additionally, If [pre_release] is
    [true], packages are updated to their latest pre-release versions. *)

val dependencies : t -> Path.project -> Path.package -> (Path.project * Path.package) list
(** [dependencies ws proj pkg] returns all of the dependencies of [pkg] from
    [proj] in [ws] in build order. *)

val parse : t -> Path.project -> Path.package -> Ast.file list
(** [parse ws proj pkg] parses the sources of [pkg] from [proj] in [ws]. *) *)
