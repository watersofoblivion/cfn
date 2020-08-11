open Format

(**
 * {1 Semantic Versions}
 *
 * A helper library for handling {{: https://semver.org/} semantic versions}.
 *)

type t
(** A semantic version *)

val semver : int -> int -> int -> string list -> string list -> t
(** [semver major minor patch pre_release build_info] constructs a new semantic
    version.  Returns {!Invalid_argument} if the given values do not constitute
    a valid semantic version. *)

val of_string : string -> t
(** [of_string str] parses [str] as a semantic version.  Raises
    {!Invalid_argument} if [str] is not a valid semver string. *)

val compare : t -> t -> int
(** [compare semver semver'] compares two semantic versions.  Returns a negative
    value if [semver < semver'], a positive value if [semver > semver'], or [0]
    if [semver = semver']. *)

val format : formatter -> t -> unit
(** [format fmt semver] prints [semver] to [fmt]. *)

val to_string : t -> string
(** [to_string semver] returns a string representation of [semver]. *)

(**
 * {2 Compatibility Groups}
 *
 * Groups semantic versions by their compatibility (e.g., major version numbers)
 * and provides utilities for working with the groups.
 *)

type 'a cg
(** A group of compatible versions *)

val empty : 'a cg
(** [empty] returns an empty compatibility group. *)

val add : t -> 'a -> 'a cg -> 'a cg
(** [add semver v grp] adds the version [semver] to the compatibility group
    [cg] and associates [v] with it.  Duplicate versions are ignored. *)

val find : t -> 'a cg -> 'a
(** [find semver grp] finds the value associated with [semver] in [grp].  Raises
    {!Not_found} if no value is associated with [semver] in [grp]. *)

val latest : int -> 'a cg -> t
(** [latest major grp] returns the latest released version in [grp] with major
    version [major].  Raises {!Not_found} if [grp] contains no released version
    with major version [major]. *)

val latest_prerelease : int -> 'a cg -> t
(** [latest_prerelease int grp] returns the latest version in [grp] with major
    version [major], including pre-releases.  Raises {!Not_found} if [grp]
    contains no version with major version [major]. *)
