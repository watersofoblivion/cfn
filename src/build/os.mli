(**
 * {1 Operating System Helpers}
 *)

(**
 * {2 Directory Handling}
 *)

val mkdir : string -> unit
(** [mkdir dir] recursively creates [dir] and any parent directories.  All
    created directories have mode 0o755.  Equivalent to the [mkdir -p] shell
    command. *)

val rmdir : string -> unit
(** [rmdir dir] recursively removes [dir] and any files or subdirectories it
    contains.  Equivalent to the [rm -rf] shell command. *)

val files : string -> string list
(** [files dir] returns a list of all the file names within [dir].  Returned
    names do not include the directory. *)

val subdirs : string -> string list
(** [subdirs dir] returns a list of all of the subdirectory names
    (non-recursive) within [dir].  Returned names do not include the directory. *)

val in_dir : string -> ('a -> 'b) -> 'a -> 'b
(** [in_dir dir fn x] changes to the directory [dir], applies [fn] to [x], and
    returns to the previous directory.  Any exception raised during the
    application of [fn] is re-reaised after returning to the previous directory. *)

val temp_dir : unit -> string
(** [temp_dir ()] creates a temporary directory and returns its path. *)

val in_temp_dir : ('a -> 'b) -> 'a -> 'b
(** [in_temp_dir fn x] changes to a fresh temporary directory and applies [fn]
    to [x].  The temporary directory is removed after [fn] is applied.  Any
    exception raised during the application of [fn] is re-raised after removing
    the directory. *)

(**
 * {2 Atomic File Operations}
 *)

val read : (in_channel -> 'a) -> string -> 'a
(** [read read_fn path] reads the file at [path] using [read_fn].  The file is
    opened in read-only binary mode and is automatically closed when [read_fn]
    returns, either normally or with an exception. *)

val write : (out_channel -> unit) -> int -> string -> unit
(** [write write_fn mode path] atomically writes a new file with mode [mode] to
    [path] using [write_fn].  The output channel is opened in write-only binary
    mode and closed when [write_fn] returns.  If [write_fn] raises an exception,
    the file is not created.  Fails when [path] already exists. *)

val overwrite : (out_channel -> unit) -> int -> string -> unit
(** [overwrite write_fn mode path] atomically creates or overwrites a file at
    [path] using [write_fn].  The output channel is opened in write-only binary
    mode and closed when [write_fn] returns.  The mode of the file is set to
    [mode].  If [write_fn] raises an exception, the original file is left
    unaltered. *)

(**
 * {2 Filesystem Searching}
 *)

val which : string -> string
(** [which exe] searches the [${PATH}] environment variable for an executable
    named [exe].  Returns the full path to the executable if found, or raises
    {!Not_found} otherwise. *)

val find_in_path : string -> string -> string
(** [find_in_path filename from] searches for a file named [filename] in the
    directory [from] and working upwards.  Returns the path to the file if it is
    found, or raises {!Not_found} otherwise. *)
