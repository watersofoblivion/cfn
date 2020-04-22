(**
 * {1 Projects}
 *)

type dependency = private {
  import_path: string; (** The import path of the dependency *)
  version:     string; (** The version of the dependency *)
  direct:      bool    (** Whether or not this dependency is required by the top-level project *)
}
(** A dependency *)

type prj = private {
  import_path:  string;         (** The import path of the project *)
  dependencies: dependency list (** The dependencies of the project *)
}
(** A project *)
