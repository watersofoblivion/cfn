(**
 * {1 Filesystem}
 *)

type dir
(** A directory *)

type file
(** A filename *)

type ext
(** A file extension *)

type path
(** A path to a file on disk *)

(**
 * {2 Directories}
 *)

val dir : string -> dir
(** [dir path] constructs a directory at [path].  Does not create the directory
    on disk. *)

val mkdir : dir -> unit
val rmdir : dir -> unit

val files : dir -> file list
val subdirs : dir -> dir list

val in_dir : dir -> ('a -> 'b) -> 'a -> 'b

val temp_dir : unit -> dir
val in_temp_dir : ('a -> 'b) -> 'a -> 'b

(**
 * {2 Files}
 *)

val file : string -> file
(** [file name] constructs a file named [name].  Does not create the file on
    disk. *)

val ext : string -> ext
(** [ext name] constructs an extension named [name]. *)

val file_ext : file -> ext
val has_ext : ext -> file -> bool
val with_ext : ext -> file -> file
val without_ext : ext -> file -> file
val replace_ext : ext -> ext -> file -> file

(**
 * {2 Paths}
 *)

val path : dir -> file -> path
(** [path dir file] constructs a path to [file] in [dir].  Does not create the
    file or directory on disk. *)

val dirname : path -> dir
val basename : path -> file

val touch : path -> unit

val with_temp_file : string -> string -> (out_channel -> unit) -> (path -> unit) -> unit

val read : (in_channel -> 'a) -> path -> 'a
(** [read read_fn path] reads the file at [path] using [read_fn].  The file is
    opened in read-only binary mode and is automatically closed when [read_fn]
    returns, either normally or with an exception. *)

val write : (out_channel -> unit) -> int -> path -> unit
(** [write write_fn mode path] atomically writes a new file with mode [mode] to
    [path] using [write_fn].  The output channel is opened in write-only binary
    mode and closed when [write_fn] returns.  If [write_fn] raises an exception,
    the file is not created.  Fails when [path] already exists. *)

val overwrite : (out_channel -> unit) -> int -> path -> unit
(** [overwrite write_fn mode path] atomically creates or overwrites a file at
    [path] using [write_fn].  The output channel is opened in write-only binary
    mode and closed when [write_fn] returns.  The mode of the file is set to
    [mode].  If [write_fn] raises an exception, the original file is left
    unaltered. *)
