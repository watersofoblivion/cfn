(**
 * {1 Import Paths}
 *)

type t = private {
  import_path: string; (** The import path proper *)
  recursive:   bool;   (** Whether or not this import path is recursive *)
  relative:    bool;   (** Whether or not this import path is a relative path *)
  github:      bool;   (** Whether or not this import path refers to a GitHub repository *)
  owner:       string; (** Repository owner if this is a GitHub import path *)
  repo:        string  (** Repository name if this is a GitHub import path *)
}

val of_string : string -> t
(** [of_string s] parses [import_path] as an import path. Raises
    {!Invalid_argument} if [import_path] cannot be parsed. *)
