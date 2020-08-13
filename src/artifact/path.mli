(**
 {1 Paths}
 *)

(**
 {2 Version Control Systems}
 *)

type vcs =
 | Git

val vcs_of_ext : string -> vcs option
(** [vcs_of_ext ext] returns a the version control system indicated by [ext], or
    [None] if the extension is not a recognized version control system. *)

val vcs_ext : vcs -> string
(** [vcs_ext vcs] returns the extension matching [vcs]. *)

(**
 {2 Names}
 *)

type id
(** A project name *)

val id : string -> id
(** [id str] parses [str] into a bare project name, suitable for initializing a
    project.  Raises {!Invalid_argument} if [str] is not a valid project
    name. *)

val name : id -> string
(** [name id] returns [id] as a string. *)

(**
 {2 Projects}
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

val vcs : project -> vcs
(** [vcs prj] returns the version control system used by [prj].  Raises
    {!InternalProject} if [prj] points to the current project. *)

val major : project -> int
(** [major prj] returns the major version of the project.  Raises
    {!InternalProject} if [prj] points to the current project. *)

val compare_project : project -> project -> int
(** [compare_project prj prj'] compares [prj] and [prj'] and returns [-1] if
    [prj] is less than [prj'], [1] if [prj] is greater than [prj'], or [0]
    otherwise. *)

(**
 {2 Packages}
 *)

type package
(** A package path *)

val package : string -> package
(** [package str] parses [str] into a package path.  Raises {!Invalid_argument}
    if [str] is not a valid package path. *)

val path : package -> string
(** [path pkg] returns the path of [pkg].  Can be blank. *)

val compare_package : package -> package -> int
(** [compare_package pkg pkg'] compares [pkg] and [pkg'] and returns a negative
    number if [pkg] is less than [pkg'], a positive number if [pkg] is greater
    than [pkg'], or [0] otherwise. *)

(**
 {2 Import}
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
