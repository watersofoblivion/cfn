(**
 * {1 Operating System Helpers}
 *)

val mkdir : string -> unit
val rmdir : string -> unit
val files : string -> string list
val subdirs : string -> string list
val in_dir : string -> ('a -> 'b) -> 'a -> 'b
val temp_dir : string -> string
val in_temp_dir : ('a -> 'b) -> 'a -> 'b

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

val find_in_path : string -> string -> string -> string
val which : string -> string
