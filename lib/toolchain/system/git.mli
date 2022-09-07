(**
 Git
*)

(**
 {1 Repositories}
 *)

type r
(**
  A Git repository

  @since 1.0
*)

val repo : string -> string -> r
(**
  Create a reference to a repository.  Does not actually create the repository
  on disk, only a reference to one.

  @param root The root of the repository
  @param default_branch The default branch of the repository
  @return A repository reference
  @since 1.0
*)

val root : r -> string
(**
  Get the root directory of a repository.

  @param repo The repository reference
  @return The root of the repository
  @since 1.0
*)

val default_branch : r -> string
(**
  Get the default branch of a repository.

  @param repo The repository reference
  @return The default branch of the repository
  @since 1.0
*)

(**
 {1 Command}
 *)

val git : r -> string list -> (bytes -> 'a) -> 'a
(**
  Run the [git] command in a repository.

  @param repo The repository reference
  @param args The command-line arguments
  @param handler Called with the standard output on success
  @return The result of the handler
  @raise Error Raised if the command exits with a non-zero exit code
  @since 1.0
*)

(**
 {1 Operations}
 *)

val clone : Uri.t -> string -> r
(**
  Clone a repository into a local directory.

  @param uri The URI of the origin remote
  @param dir The directory to clone into
  @return A repository reference
  @raise Invalid_argument Raised if the directory is not an absolute path
  @since 1.0
*)

val fetch : r -> unit
(**
  Fetch new commits from all remotes.

  @param repo The repository reference
  @since 1.0
*)

val versions : r -> string Semver.cg
(**
  Detects all available versions within a repository, based on branch and tag
  names.

  @param repo The repository reference
  @return A semantic version compatibility group
  @since 1.0
*)

val checkout : r -> string -> unit
(**
  Check out a specific reference in a repository.

  @param repo The repository reference
  @param gitref The git reference to check out
  @raise Invalid_argument Raised if the reference is not a valid git reference
    in the repository
  @since 1.0
*)
