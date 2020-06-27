(**
 * {1 Paths}
 *)

(**
 * {2 Projects}
 *)

type project
(** A project path. *)

exception InternalProject
(** Raised when an unsupported operation is performed on an internal project. *)

val project : string -> project
(** [project str] parses [str] into a project path.  Raises
    {!InvalidProjectPath} if [str] is not a valid project path. *)

val current : project -> bool
(** [current prj] returns [true] if [prj] points to the current project, or
    [false] otherwise. *)

val source : project -> string
(** [source prj] returns the source for the project.  Raises {!InternalProject}
    if [prj] points to the current project. *)

val major : project -> int
(** [major prj] returns the major version of the project.  Raises
    {!InternalProject} if [prj] points to the current project. *)

(**
 * {2 Packages}
 *)

type package
(** A package path *)

val package : string -> package
(** [package str] parses [str] into a package path.  Raises {!Invalid_argument}
    if [str] is not a valid package path. *)

val path : package -> string
(** [path pkg] returns the path of [pkg].  Can be blank. *)

(**
 * {2 Import}
 *)

type import
(** An import path *)

val import : string -> import
(** [import str] parses [str] into an import path.  Raises {!Invalid_argument}
    if [str] is not a valid import path. *)

val prj : import -> project
(** [prj impt] returns the project path portion of [impt]. *)

val pkg : import -> package
(** [pkg impt] returns the package path portion of [impt]. *)

val recursive : import -> bool
(** [recursive impt] returns [true] if [impt] is recursive, or [false]
    otherwise. *)
