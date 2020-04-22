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

type cg
(** A group of compatible versions *)

val compatibility_groups : t list -> cg list
(** [compatability_groups semvers] groups [semvers] by their compatibility.
     Groups are returned in order of increasing major versions. *)

val major : cg -> int
(** [major cg] returns the common major version number of all of the versions in
    [cg]. *)

val versions : cg -> t list
(** [versions cg] returns all of the individual versions in [cg]. *)

val newest : cg -> t
(** [newest cg] returns the newest version in [cg]. *)
