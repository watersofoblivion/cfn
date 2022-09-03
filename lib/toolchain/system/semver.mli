open Format

(**
 {1 Semantic Versions}

 A helper library for handling {{: https://semver.org/} semantic versions}.
 *)

type t
(**
  A semantic version

  @since 1.0
*)

val semver : int -> int -> int -> string list -> string list -> t
(**
  Construct a new semantic version.

  @param major The major version
  @param minor The minor version
  @param patch The patch version
  @param pre_release The pre-release segments
  @param build_info The build information segments
  @return A semantic version
  @raise Invalid_argument Raised if the given values do not constitute a valid
    semantic version.
  @since 1.0
*)

val major : t -> int
(**
  Get the major version of a semantic version.

  @param semver The semantic version
  @return The major version
  @since 1.0
*)

val minor : t -> int
(**
  Get the minor version of a semantic version.

  @param semver The semantic version
  @return The minor version
  @since 1.0
*)

val patch : t -> int
(**
  Get the patch version of a semantic version.

  @param semver The semantic version
  @return The patch version
  @since 1.0
*)

val prerelease : t -> string list
(**
  Get the pre-release segments of a semantic version.

  @param semver The semantic version
  @return The pre-release segments
  @since 1.0
*)

val build_info : t -> string list
(**
  Get the build information segments of a semantic version.

  @param semver The semantic version
  @return The build information segments
  @since 1.0
*)

val of_string : string -> t
(**
  Parse a semantic version from a string.

  @param str The semantic version string
  @return A semantic version
  @raise Invalid_argument Raised if the string is not a valid semantic version
  @since 1.0
*)

val compare : t -> t -> int
(**
  Compare two semantic versions, following the rules given in the semantic
  version specification.

  @param semver The first semantic version
  @param semver' The second semantic version
  @return A negative value if the first version is less than the second, a
    positive value if the first version is greater than the second, or [0] if
    the two versions are equal
  @since 1.0
*)

val format : formatter -> t -> unit
(**
  Pretty-print a semantic version to a formatter.

  @param fmt The formatter to print to
  @param semver The version to print
  @since 1.0
*)

val to_string : t -> string
(**
  Format a semantic version as a string.

  @param semver The version to format
  @return The version as a string
  @since 1.0
*)

(**
 {2 Compatibility Groups}

 Groups semantic versions by their compatibility (e.g., major version numbers)
 and provides utilities for working with the groups.
 *)

type 'a cg
(**
  A group of compatible versions

  @since 1.0
*)

val empty : 'a cg
(**
  The empty compatibility group.

  @since 1.0
*)

val add : t -> 'a -> 'a cg -> 'a cg
(**
  Add a sematic version to a group non-destructively and associates a value with
  it.  Duplicate versions are ignored.

  @param semver The version to add to the group
  @param v The value to associate with the version
  @param grp The group to add the version to
  @return The group extended with the version
  @since 1.0
*)

val find : t -> 'a cg -> 'a
(**
  Find the value associated with a sematic version in a group.

  @param semver The version to find
  @param grp The group to search
  @return The value associated with the version
  @raise Not_found Raised if the version is not bound in the group
  @since 1.0
*)

val latest : int -> 'a cg -> t
(**
  Get the latest released version in a group that is compatible with a major
  version.

  @param major The major version
  @param grp The group to search in
  @return The latest released version compatible with the major version
  @raise Not_found Raised if the group does not contain a compatible released
    version
  @since 1.0
*)

val latest_prerelease : int -> 'a cg -> t
(**
  Get the latest version in a group that is compatible with a major version,
  including pre-relase versions.

  @param major The major version
  @param grp The group to search in
  @return The latest version compatible with the major version
  @raise Not_found Raised if the group does not contain a compatible version
  @since 1.0
*)
