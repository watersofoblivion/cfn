(**
 * {1 Workspace}
 *
 * The build
 *)

(**
 * {2 Constants}
 *)

val project_file_name : string
(** [project_file_name] is the name of the project file *)

val lock_file_name : string
(** [lock_file_name] is the name of the lock file. *)

val cache_dir : string
(** [cache_dir] is the directory containing downloaded sources and build
    artifacts. *)

val src_dir : string
(** [src_dir] is the directory within [cache_dir] that contains downloaded
    sources. *)

val lib_dir : string
(** [lib_dir] is the directory within [cache_dir] that contains compiled package
    artifacts. *)

val pkg_dir : string
(** [pkg_dir] is the directory within [cached_dir] that contains fully-packaged
    applications to be deployed to AWS. *)

(**
 * {2 Types}
 *)

type t
(** A workspace. *)

type project
(** A project in a workspace, either the current project or a dependency. *)

type package
(** A package within a project in a workspace. *)

(**
 * {2 Workspaces}
 *)

val current : unit -> t
(** [current _] returns the workspace for the current project.  Values set from
    the environment. *)

val create : Path.project -> string -> t
(** [create prj dir] creates a new workspace for project [prj] in directory
    [dir].  Initializes a Git repository and performs an initial commit. *)

(*
val fetch : Path.project -> t -> t
(** [fetch path ws] fetches the project sources at [path] and adds them to the
    workspace [ws].  Returns a new workspace with the sources added, or raises
    {!Not_found} if the sources could not be fetched. *)

val docker : t -> string list -> unit
(** [docker t args] runs the Docker executable with command-line arguments
    [args].  Raises {!Not_found} if the executable is not set or
    {!Invalid_argument} if the path is not set to a valid executable. *)

val dockerfile : t -> string
(** [dockerfile t] returns the path to the build Dockerfile in the CFN++ home
    directory.  Raises {!Not_found} if the CFN++ home directory is not set or
    {!Invalid_argument} if the path is not set to a valid file. *)

val stdlib : t -> Path.package -> package
(** [stdlib t path] returns the standard library package pointed to by [path].
    Raises {!Not_found} if the CFN++ home directory is not set or if the package
    does not exist, or {!Invalid_argument} if the path is not set to a valid
    directory. *) *)

(**
 * {2 Projects}
 *)
(*
val major : project -> int
(** [major prj] returns the project's major version. *)

val version : project -> Semver.t
(** [version prj] returns the current version of the project.  Raises
    {!Not_found} if the project has not been fetched. *)

val tagged_versions : project -> Semver.t list
(** [tagged_versions prj] returns a list of versions that have been tagged in
    the repository.  Raises {!Not_found} if the project has not been fetched. *)

val major_branches : project -> int list
(** [major_branches prj] returns a list of branches named for major versions.
    Raises {!Not_found} if the project has not been fetched. *)

val packages : project -> string list
(** [packages prj] returns the list of the packages within the project.
    Returns {!Not_found} if the project has not been fetched. *) *)

(**
 * {2 Packages}
 *)
(*
val scan : package -> package
(** [scan pkg] scans the package and returns a populated package.  Raises
   {!Not_found} if the containing package has not been fetched. *)

val imports : package -> unit
(** [imports pkg] scans the sources and returns the projects and packages
    imported.  Raises {!Not_found} if the containing package has not been
    fetched. *)

val parse : package -> unit
(** [parse pkg] parses and returns the source files.  Raises {!Not_found}) if
    the containing project has not been fetched. *)

val name : package -> string
(** [name pkg] returns the declared name of the package.  Raises {!Not_found} if
    the sources have not been scaned to detect the package name. *)

val up_to_date : package -> bool
(** [up_to_date pkg] returns true if the package's compiled artifacts are up to
    date with the sources. *)

val anf : package -> unit
(** [artifacts_anf art] ... .  Raises {!Not_found} if the package has not been
    compiled. *) *)

(**
 * {2 Lock File}
 *)
(*
val lockfile : t -> lockfile
(** [lockfile ws] ... . *) *)
