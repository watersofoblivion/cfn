(**
 * {1 Workspace}
 *
 * The build
 *)

(**
 * {2 Types}
 *)

type ctx
(** The context describing a workspace. *)

type project
(** A project in a workspace, either the current project or a dependency. *)

type package
(** A package within a project in a workspace. *)

type lockfile
(** A file containing version information about all the projects in a
    workspace. *)

(**
 * {2 Workspaces}
 *)

val ctx : ctx
(** [ctx] returns a context set with values from the environment. *)

val docker : ctx -> string list -> unit
(** [docker ctx args] runs the Docker executable with command-line arguments
    [args].  Raises {!Not_found} if the executable is not set or
    {!Invalid_argument} if the path is not set to a valid executable. *)

val dockerfile : ctx -> string
(** [dockerfile ctx] returns the path to the build Dockerfile in the CFN++ home
    directory.  Raises {!Not_found} if the CFN++ home directory is not set or
    {!Invalid_argument} if the path is not set to a valid file. *)

val stdlib : ctx -> Path.package -> package
(** [stdlib ctx path] returns the standard library package pointed to by [path].
    Raises {!Not_found} if the CFN++ home directory is not set or if the package
    does not exist, or {!Invalid_argument} if the path is not set to a valid
    directory. *)

(**
 * {2 Projects}
 *)

val project : ctx -> Path.project -> project
(** [project ws path] constructs a project in [ws] with path [path]. *)

val fetch : project -> project
(** [fetch prj] fetches the project sources. *)

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
    Returns {!Not_found} if the project has not been fetched. *)

(**
 * {2 Packages}
 *)

val package : project -> Path.package -> package
(** [package prj path] constructs a package in [prj] under path [path]. *)

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
    compiled. *)

(**
 * {2 Lock File}
 *)

val lockfile : ctx -> lockfile
(** [lockfile ws] ... . *)
