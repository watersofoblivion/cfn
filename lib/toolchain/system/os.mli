(**
 Operating System Helpers
 *)

(**
 {1 Directory Handling}
 *)

val mkdir : string -> unit
(**
  Recursively create a directory and any parent directories.  All created
  directories have mode 0o755.  Equivalent to the [mkdir -p] shell command.

  @param dir The directory to create
  @since 1.0
*)

val rmdir : string -> unit
(**
  Recursively remove a directory and any files or subdirectories it contains.
  Equivalent to the [rm -rf] shell command.

  @param dir The directory to remove
  @since 1.0
*)

val files : string -> string list
(**
  List the files in a directory.  The returned names are relative to the listed
  directory.

  @param dir The directory to list
  @return A list of file names
  @since 1.0
*)

val dirs : string -> string list
(**
  List the subdirectory names (non-recursive) within a directory.  The returned
  names are relative to the listed directory.

  @param dir The directory to list
  @return A list of directory names
  @since 1.0
*)

val subdirs : string -> string list
(**
  Recursively list all subdirectories within a directory.  The returned names
  are relative to the listed directory.

  @param dir The directory to list
  @return A list of subdirectories
  @since 1.0
*)

val in_dir : string -> ('a -> 'b) -> 'a -> 'b
(**
  Changes to a directory, applies a function, and changes directory back to the
  original directory.  Any exception raised during the application is re-raised
  after returning to the original directory.

  @param dir The directory to change to
  @param fn The function to apply
  @param x The value to apply the function to
  @return The result of applying the function
  @since 1.0
*)

val temp_dir : unit -> string
(**
  Create a temporary directory.

  @return The path of the temporary directory
  @since 1.0
*)

val in_temp_dir : ('a -> 'b) -> 'a -> 'b
(**
  Creates and changes to a fresh temporary directory, applies a function, then
  changes back to the original directory and removes the temporary directory.
  Any exception raised during the application of the function is re-raised after
  removing the temporary directory.

  @param fn The function to apply
  @param x The value to apply the function to
  @return The result of applying the function
  @since 1.0
*)

(**
 {1 Atomic File Operations}
 *)

val read : (in_channel -> 'a) -> string -> 'a
(**
  Read a file.  The file is opened in read-only binary mode and is automatically
  closed when the reader returns, either normally or with an exception.  If the
  reader function raises an exception, it is re-raised after the file is closed.

  @param read_fn The reader function
  @param path The path of the file to read
  @return The result of the reader function
  @since 1.0
*)

val write : (out_channel -> unit) -> int -> string -> unit
(**
  Atomically write a new file.  The file is opened in write-only binary mode and
  closed when the writer function returns, either normally or with an exception.
  If the writer function raises an exception, the new file is not created and
  the exception is re-raised.  Fails if the output path already exists.

  @param write_fn The writer function
  @param mode The mode of the file
  @param path The path of the file
  @since 1.0
*)

val overwrite : (out_channel -> unit) -> int -> string -> unit
(**
  Atomically write or overwrite a file.  The file is opened in write-only binary
  mode and closed when the writer function returns, either normally or with an
  exception.  If the writer function raises an exception, the new file is not
  created or the existing file is left un-altered and the exception is
  re-raised.  Fails if the mode of an existing file does not match the mode
  given.

  @param write_fn The writer function
  @param mode The mode of the file
  @param path The path of the file
  @since 1.0
*)

(**
 {1 Filesystem Searching}
 *)

val which : string -> string
(**
  Search the [${PATH}] environment variable for an executable.

  @param exe The executable to find
  @return The full path to the executable
  @raise Not_found Raised if the executable is not on the path
  @since 1.0
*)

val find_in_path : string -> string -> string
(**
  Searches for a file in a directory or any parent directories, working up the
  filesystem hierarchy.

  @param filename The name of the file to find
  @param from The directory to start searching in
  @return The path to the file
  @raise Not_found If the file does not exist in the directory tree
  @since 1.0
*)

(**
 {1 Process Control}
 *)

type output
(**
  The output from a process

  @since 1.0
*)

val of_stdout : bytes -> output
(**
  Contruct an output from standard output.

  @param bs The bytes of the standard output
  @return An output
  @since 1.0
*)

val of_stderr : bytes -> output
(**
  Contruct an output from standard error.

  @param bs The bytes of the standard error
  @return An output
  @since 1.0
*)

val stdout : output list -> bytes
(**
  Extract the standard output.

  @param output The output of a process
  @return The standard output
  @since 1.0
*)

val stderr : output list -> bytes
(**
  Extract the standard error.

  @param output The output of a process
  @return The standard error
  @since 1.0
*)

val combined : output list -> bytes
(**
  Get the combined standard output and standard error of a process.

  @param output The output
  @return The combined output
  @since 1.0
*)

val dump : out_channel -> out_channel -> output list -> unit
(**
  Dump the output of a process to a standard output and a standard error
  channel.

  @param stdout The channel to write standard output to
  @param stderr The channel to write standard error to
  @param process_output The process output
  @since 1.0
*)

exception NonZero of {
  status: int;         (** The process's exit status *)
  output: output list; (** The process's output *)
}
(**
  Raised when a process exits with a non-zero status code.

  @since 1.0
*)

val lines : bytes -> string list
(**
  Split bytes into lines.  Trim any leading or trailing blank lines.

  @param bs The bytes
  @return A list of lines
  @since 1.0
*)

val first_line : bytes -> string
(**
  Get the first line from a multi-line output.

  @param bs The output bytes
  @return The first line
  @raise Invalid_argument Raised if the output contains no non-blank lines
  @since 1.0
*)

val line : bytes -> string
(**
  Reads a one-line output.

  @param bs The output bytes
  @return The output line
  @raise Invalid_argument Raised if the output contains more than one line
  @since 1.0
*)

val ignore : bytes -> unit
(** [ignore bs] ignores the output [bs].
  Ignore output.

  @param bs The output to ignore
  @since 1.0
*)

val run : string -> string list -> (bytes -> 'a) -> 'a
(**
  Run an external process.  The command name is searched for in the path.

  @param cmd The command name
  @param args The command-line arguments
  @param handler Called with the output of the process
  @return The result of the handler
  @raise Not_found Raised if the executable cannot be found on the path
  @raise NonZero Raised if the process exited with a non-zero exit status
  @since 1.0
*)
