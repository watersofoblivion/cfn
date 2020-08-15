(**
 {1 Git}
 *)

(**
 {2 Repositories}
 *)

type r
(** A Git repository *)

val repo : string -> string -> r
(** [repo root default_branch] constructs a repository rooted at [root] with a
    default branch name of [default_branch].  Does not create a repository
    on-disk, only a reference to one. *)

val root : r -> string
(** [root repo] returns the root directory containing a checked out copy of
    [repo]. *)

val default_branch : r -> string
(** [default_branch repo] returns the default branch of the repo. *)

(**
 {2 Command}
 *)

val git : r -> string list -> (bytes -> 'a) -> 'a
(** [git repo args handler] runs the "git" command with command-line arguments
    [args] in [repo].  On success, returns the result of passing standard output
    to [handler].  On failure (non-zero exit status), raises {!Error}. *)

(**
 {2 Operations}
 *)

val clone : Uri.t -> string -> r
(** [clone uri dir] clones the repository at [uri] to [dir].  Raises
    {!Invalid_argument} if [dir] is not an absolute path. *)

val fetch : r -> unit
(** [fetch repo] fetches new commits from all remotes of [repo]. *)

val versions : r -> string Semver.cg
(** [versions repo] detects all available versions within [repo]. *)

val checkout : r -> string -> unit
(** [checkout repo gitref] checks out [gitref] in [repo].  Raises
    {!Invalid_argument} if [ref] does not point to a valid git reference. *)
