(**
 * {1 Docker}
 *
 * A wrapper around the Docker CLI.  This is used when building binaries for
 * deployment to AWS, since the output executables must run on Amazon Linux 2.
 *)

(**
 * {2 Command}
 *)

val docker : string list -> (bytes -> 'a) -> 'a
(**
  Run the [docker] commad.

  @param args The command-line arguments
  @param handler Called with the standard output on success
  @return The result of the handler
  @raise Os.NonZero Raised if the command exits with a non-zero exit code
  @since 1.0
*)

(**
 * {2 Images}
 *)

type i
(**
  A docker image

  @since 1.0
*)

val image : string -> string -> i
(**
  Construct a reference to an image with the id [<name>:<tag>].  Does not build
  the image with Docker, only creates a reference to it.

  @param name The image name
  @param tag The image tag
  @return An image reference
  @since 1.0
*)

val build : string -> string -> string -> i
(**
  Build an image from the contents of a directory.

  @param dir The directory containing the image content
  @param name The image name
  @param tag The image tag
  @return An image reference
  @since 1.0
*)

val img_id : i -> string
(**
  Get the identifer of an image, formatted as [<name>:<tag>].

  @param img A reference to the image
  @return The image identifier
  @since 1.0
*)

val img_name : i -> string
(**
  Get the name of an image.

  @param img A reference to the image
  @return The image name
  @since 1.0
*)

val img_tag : i -> string
(**
  Get the tag of an image.

  @param img A reference to the image
  @return The image tag
  @since 1.0
*)

(**
 * {2 Operations}
 *)

type mount
(**
  A mount point for a container

  @since 1.0
*)

val local_dir : mount -> string
(**
  Get the local directory of a mount point.

  @param m The mount point
  @return The local directory of the mount point
  @since 1.0
*)

val container_dir : mount -> string
(**
  Get the container directory of a mount point.

  @param m The mount point
  @return The container directory of the mount point
  @since 1.0
*)

val dir_mount : string -> string -> mount
(**
  Create a mount point from a local directory into a container.

  @param local_dir The local directory on the machine
  @param container_dir The directory in the container
  @return A mount point
  @raise Invalid_argument Raised if either directory is not an absolute path
  @since 1.0
*)

type mounts
(**
  A set of mounts in a container

  @since 1.0
*)

val mounts : mounts
(**
  The empty set of mounts.

  @since 1.0
*)

val add_mount : mount -> mounts -> mounts
(**
  Add a mount to a set of mounts.

  @param mount The mount to add
  @param mounts The set to add the mount to
  @return The set of mounts extended with the added mount
  @since 1.0
*)

val run_in : string -> string list -> ?mounts:mounts -> i -> (bytes -> 'a) -> 'a
(**
  Run a command in a new Docker container instance.

  @param cmd The command to run
  @param args The command-line arguments
  @param mounts The mounts to attach to the container
  @param img The image of the container
  @param handler Called with the standard output on success
  @return The result of the handler
  @raise Os.NonZero Raised when the command exits with a non-zero exit code
  @since 1.0
*)
