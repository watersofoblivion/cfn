open Format

(**
 * {1 Projects}
 *)

(**
 * {2 Source Locations}
 *)

type src = private
  | Internal
  | GitHub of string * string * int
  | External of string option * string * int option * string option * int
(** The source location of a project. *)

val internal_src : src
(** [internal_src] returns a source pointing at the current project. *)

val github_src : string -> string -> int -> src
(** [github_src owner repo major] returns a source pointing at the GitHub
    repository [github.com/<owner>/<repo>] at major version [major].  Raises
    {!Invalid_argument} if [owner] or [repo] are blank or if [major] is
    negative. *)

val external_src : string option -> string -> int option -> string option -> int -> src
(** [external_src proto host port path major] returns a source pointing at the
    external repository [<proto>://<host>:<port>/<path>] at major version
    [major].  Raises {!Invalid_argument} if [proto] is blank, [host] is not a
    valid hostname, if [port] is negative, or [path] is not a relative path. *)

val of_string : string -> src
(** [of_string s] parses a source from the [s].  Raises {!Invalid_argument} if
    the source cannot be parsed. *)

val format : formatter -> src -> unit
(** [format fmt src] pretty-prints [src] to [fmt]. *)

val to_string : src -> string
(** [to_string src] returns [src] as a pretty-printed string. *)

(**
 * {2 Projects}
 *)

type t
(** A project *)

val create : Ctx.t -> src -> t
(** [create ctx src] constructs a project [prj] with source [src]. *)

val src : t -> src
(** [src prj] returns the project's source location. *)

val fetch : t -> t
(** [fetch prj] fetches the project sources. *)

val major : t -> int
(** [major prj] returns the project's major version. *)

val version : t -> Semver.t
(** [version prj] returns the version of the project.  Raises {!Not_found} if
    the project has not been fetched. *)

val tagged_versions : t -> Semver.t list
(** [tagged_versions prj] returns a list of versions that have been tagged in
    the repository.  Raises {!Not_found} if the project has not been fetched. *)

val major_branches : t -> int list
(** [major_branches prj] returns a list of branches named for major versions.
    Raises {!Not_found} if the project has not been fetched. *)

val pkg_names : t -> string list
(** [pkg_names prj] returns the list of the packages within the project.
    Returns {!Not_found} if the project has not been fetched. *)

val path : t -> string
(** [path prj] returns the directory containing the project's source or build
    artifacts relative to a build directory. *)
