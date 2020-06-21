(**
 * {1 Packages}
 *)

(**
 * {2 Packge Sources}
 *)

type src

(**
 * {2 Compiled Artifacts}
 *)

type lib

(**
 * {2 Packages}
 *)

type t
(** A package within a project *)

val create : Ctx.t -> Prj.t -> string -> t
(** [create ctx prj path] creates a package with path [path] in the project
    [prj]. *)

val scan : t -> t
(** [scan pkg] scans the package and returns a populated package.  Raises
    {!Not_found} if the containing package has not been fetched. *)

val imports : t -> unit
(** [imports pkg] scans the sources and returns the projects and packages
    imported.  Raises {!Not_found} if the containing package has not been
    fetched. *)

val parse : t -> unit
(** [parse pkg] parses and returns the source files.  Raises {!Not_found}) if
    the containing project has not been fetched. *)

val path : t -> string
(** [path pkg] returns the path of the package within its containing project. *)

val name : t -> string
(** [name pkg] returns the declared name of the package.  Raises {!Not_found} if
    the sources have not been scaned to detect the package name. *)

val src : t -> src
(** [source pkg] returns the package's sources.  Raises {!Not_found} if the
    package has not been scanned. *)

val lib : t -> lib
(** [lib pkg] returns the package's compiled artifacts.  Raises {!Not_found} if
    the package has not been scanned or the compiled artifacts do not exist. *)

val up_to_date : t -> bool
(** [up_to_date pkg] returns true if the package's compiled artifacts are up to
    date with the sources. *)
