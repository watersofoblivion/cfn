(**
 {1 Git}
 *)

(**
 {2 Repositories}
 *)

type t
(** A Git repository *)

val repo : string -> string -> t
(** [repo root default_branch] constructs a repository rooted at [root] with a
    default branch name of [default_branch].  Does not create a repository
    on-disk, only a reference to one. *)

val root : t -> string
(** [root repo] returns the root directory containing a checked out copy of
    [repo]. *)

val default_branch : t -> string
(** [default_branch repo] returns the default branch of the repo. *)

(**
 {2 Command}
 *)

val git : t -> string list -> (bytes -> 'a) -> 'a
(** [git repo args handler] runs the "git" command with command-line arguments
    [args] in [repo].  On success, returns the result of passing standard output
    to [handler].  On failure (non-zero exit status), raises {!Os.NonZero}. *)

val lines : bytes -> string list
(** [lines bs] splits [bs] into lines.  Trims any leading or trailing blank
    lines. *)

val first_line : bytes -> string
(** [first_line bs] returns just the first line from multi-line output.  Raises
    {!Invalid_argument} if [bs] contains no non-blank lines. *)

val line : bytes -> string
(** [line bs] reads one-line output from [bs].  Raises {!Invalid_argument} if
    [bs] contains more than one line. *)

val ignore : bytes -> unit
(** [ignore bs] ignores the output [bs]. *)

(**
 {2 Operations}
 *)

val clone : Uri.t -> string -> t
(** [clone uri dir] clones the repository at [uri] to [dir].  Raises
    {!Invalid_argument} if [dir] is not an absolute path. *)

val fetch : t -> unit
(** [fetch repo] fetches new commits from all remotes of [repo]. *)

val versions : t -> string Semver.cg
(** [versions repo] detects all available versions within [repo]. *)

val checkout : t -> string -> unit
(** [checkout repo gitref] checks out [gitref] in [repo].  Raises
    {!Invalid_argument} if [ref] does not point to a valid git reference. *)
