open Format

(**
 * {1 Paths}
 *
 * External: SOURCE-PATH[@VERSION]:PACKAGE-PATH (prj/path@vX:pkg/path, prj/path@vX:..., prj/path@vX:pkg/path/...)
 * Internal: .:[PACKAGE-PATH] (.:pkg/path, .:..., .:pkg/path/...)
 * Stdlib: PACKAGE-PATH (pkg/path)
 *)

type src = private
  | Internal
  | GitHub of string * string * int
  | External of string option * string * int option * string option * int

val internal : src
(** [internal] returns the current project as a source. *)

val github : string -> string -> int -> src
(** [github owner repo] constructs a GitHub project source pointing to the repo
    [github.com/<owner>/<repo>] at major version [ver].  Raises
    {!Invalid_argument} if either [owner] or [repo] are blank, or if the version
    number is negative. *)

val ext : string option -> string -> int option -> string option -> int -> src
(** [external proto host port path ver] constructs an external project source
    pointing to the URI [proto]://[host]:[port]/[path] at major version [ver].
    Raises {!Invalid_argument} if [host] is blank or does not end with
    [.\[a-z\]+], if [path] is not a relative path (or blank), or if the version
    number is negative. *)

type t = private {
  source:    src;    (** Source path *)
  package:   string; (** The package within the project *)
  recursive: bool;   (** Whether or not this path is recursive *)
}

val create : src -> string -> bool -> t
(** [create src pkg recur] construct a project path with source [src] pointing
    to package [pkg] and all nested packages if [recur] is true. *)

val of_string : string -> t
(** [of_string str] parses [str] into a project path. Raises {!Invalid_argument}
    if [str] cannot be parsed. *)

val format : formatter -> t -> unit
(** [format fmt path] formats [path] to [fmt]. *)

val to_string : t -> string
(** [to_string path] formats the project path [path] as a string. *)
