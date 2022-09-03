(**
 {1 Paths}
 *)

(**
 {2 Version Control Systems}
 *)

type vcs =
 | Git
(**
  Supported version control systems

  @since 1.0
*)

val vcs_of_ext : string -> vcs option
(**
  Detect a version control system based on a file extension.

  @param ext The extension
  @return The recognized version control system, or [None] if the extension is
    not a recognized version control system
  @since 1.0
*)

val vcs_ext : vcs -> string
(**
  Get the extension for a version control system.

  @param vcs The version control system
  @return The extension for the system
  @since 1.0
*)

(**
 {2 Names}
 *)

type id
(**
  A project name

  @since 1.0
*)

val id : string -> id
(**
  Parse a string into a bare project name, suitable for initializing a project.

  @param str The string to parse
  @return A project name
  @raise Invalid_argument Raised if the string is not a valid project name
  @since 1.0
*)

val name : id -> string
(**
  Get a project name as a string.

  @param id The project name
  @return The name as a string
  @since 1.0
*)

(**
 {2 Projects}
 *)

type project
(**
  A project path.

  @since 1.0
*)

exception InternalProject
(**
  Raised when an unsupported operation is performed on an internal project.

  @since 1.0
*)

val project : string -> project
(**
  Parse a string into a project path.

  @param str The path string
  @return A project path
  @raise InvalidProjectPath Raised if the string is not a valid project path
  @since 1.0
*)

val current : project -> bool
(**
  Test if a project path points to the current project.

  @param prj The project path
  @return [true] if the path points to the current project, [false] otherwise
  @since 1.0
*)

val source : project -> string
(**
  Get the source for the project.

  @param prj The project path
  @return The source for the project
  @raise InternalProject Raised if the project points to the current project
  @since 1.0
*)

val vcs : project -> vcs
(**
  Get the version control system used by a project.

  @param prj The project path
  @return The version control system used by the project
  @raise InternalProject Raised if the project points to the current project
  @since 1.0
*)

val major : project -> int
(**
  Get the major version of a project.

  @param prj The project path
  @return The major version of the project
  @raise InternalProject Raised if the project points to the current project
  @since 1.0
*)

val compare_project : project -> project -> int
(**
  Compare two projects.  The ordering is arbitry and used to fulfill interface
  requiremnts for constructing sets and maps of projects.

  @param prj The first project
  @param prj' The second project
  @return A negative value if the first project is less than the second, a
    positive value if the first project is greater than the second, or [0] if
    the two projects are equal
  @since 1.0
*)

(**
 {2 Packages}
 *)

type package
(**
  A package path

  @since 1.0
*)

val package : string -> package
(**
  Parse a string into a package path.

  @param str The string to parse
  @return A package path
  @raise Invalid_argument Raised if the string is not a valid package path
  @since 1.0
*)

val path : package -> string
(**
  Get the path of a package.  Can return a blank path.

  @param pkg The package
  @return The path of the package
  @since 1.0
*)

val compare_package : package -> package -> int
(**
  Compare two packages.  The ordering is arbitry and used to fulfill interface
  requiremnts for constructing sets and maps of packages.

  @param pkg The first package
  @param pkg' The second package
  @return A negative value if the first package is less than the second, a
    positive value if the first package is greater than the second, or [0] if
    the two packages are equal
  @since 1.0
*)

(**
 {2 Import}
 *)

type import
(**
  An import path

  @since 1.0
*)

val import : string -> import
(**
  Parse a string into an import path.

  @param str The string to parse
  @return An import path
  @raise Invalid_argument Raised if the string is not a valid import path
  @since 1.0
*)

val prj : import -> project
(**
  Get the project path of an import path.

  @param impt The import path
  @return The project path
  @since 1.0
*)

val pkg : import -> package
(**
  Get the package path of an import path.

  @param impt The import path
  @return The package path
  @since 1.0
*)

val recursive : import -> bool
(**
  Test if an import path is recursive.

  @param impt The import path
  @return [true] if the path is recursive, [false] otherwise
  @since 1.0
*)
