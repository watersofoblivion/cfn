(**
 * {1 Packages}
 *)

type pkg = private {
  package_path:     string;      (** The path of the package relative to the project root *)
  package_name:     string;      (** The name of the package *)
  src_path:         string;      (** The directory containing the source of the package *)
  cfn_files:        string list; (** The CFN++ source files in this package *)
  other_files:      string list; (** The non-CFN++ source files in this package *)
  last_updated:     float;
}
(** A package within a project *)

val pkg : Ctx.t -> ImportPath.t -> string -> pkg
(**  *)

val pkg_imports : Ctx.t -> ImportPath.t -> string -> (string * string list) list
(**  *)
