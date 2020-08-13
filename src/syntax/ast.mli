(**
 {1 Abstract Syntax}
 *)

(**
 {2 Package Statement}
 *)

(** A package statement *)
type package_stmt = private {
  package_stmt_loc: Loc.t; (** Location of the statement *)
  name_loc:         Loc.t; (** Location of the name *)
  pkg_name:         string (** Name of the package *)
}

val package_stmt : Loc.t -> Loc.t -> string -> package_stmt
(** [package_stmt kwd_loc name_loc pkg_name] constructs a package statement
    where the [package] keyword is located at [kwd_loc], the package name is
    located at [name_loc], and the package name is [pkg_name]. *)

val deloc_package_stmt : package_stmt -> package_stmt
(** [deloc_package_stmt package_stmt] strips all location information from
    [package_stmt], replacing it with {!Loc.dummy}. *)

(**
 {2 Imports}
 *)

(** The [from] clause in an import statement. *)
type from_clause = private {
  from_clause_loc: Loc.t; (** Location of the "from" clause *)
  ip_loc:          Loc.t; (** Location of the import path *)
  import_path:     string (** Import path of the project *)
}

val from_clause : Loc.t -> Loc.t -> string -> from_clause
(** [from_clause kwd_loc ip_loc import_path] constructs a [from] clause in an
    import statement where the [from] keyword is located at [kew_loc], the
    import path is located at [ip_loc], and the import path is [import_path]. *)

val deloc_from_clause : from_clause -> from_clause
(** [deloc_from_clause from_clause] strips all location information from
    [from_clause], replacing it with {!Loc.dummy}. *)

type package_alias = private {
  package_alias_loc: Loc.t; (** The location of the alias name *)
  local_alias:       string (** The alias of the package *)
}

val package_alias : Loc.t -> string -> package_alias
(** [package_alias loc alias] constructs a package alias where the alias is
    located at [loc] and the name of the alias is [alias]. *)

val deloc_package_alias : package_alias -> package_alias
(** [deloc_package_alias package_alias] strips all location information from
    [package_alias], replacing it with {!Loc.dummy}. *)

(** An individual package import in an import statement. *)
type package_clause = private {
  package_clause_loc: Loc.t;               (** The location of the package clause *)
  pp_loc:             Loc.t;               (** The location of the package path *)
  package_path:       string;              (** The package path, relative to the import path *)
  alias:              package_alias option (** An optional alias for the imported package *)
}

val package_clause : Loc.t -> string -> package_alias option -> package_clause
(** [package_clause pp_loc package_path alias] constructs a package clause where
    [pp_loc] is the location of the package path, and [package_path] is the
    package path, and [alias] is the optional local alias of the package. *)

val deloc_package_clause : package_clause -> package_clause
(** [deloc_package_clause package_clause] strips all location information from
    [package_clause], replacing it with {!Loc.dummy}. *)

(** The [import] clause in an import statement. *)
type import_clause = private {
  import_clause_loc: Loc.t;              (** The location of the import clause *)
  packages:          package_clause list (** The packages to import from that project, with optional local names *)
}

val import_clause : Loc.t -> package_clause list -> import_clause
(** [import_clause kwd_loc packages] constructs an import clause where [kwd_loc]
    is the location of the [import] keyword and [packages] are the package
    clauses defining the imports. *)

val deloc_import_clause : import_clause -> import_clause
(** [deloc_import_clause import_clause] strips all location information from
    [import_clause], replacing it with {!Loc.dummy}. *)

(** An import statement *)
type import_stmt = private {
  import_stmt_loc: Loc.t;              (** The location of the import statement *)
  from:            from_clause option; (** The "from" clause *)
  import:          import_clause       (** The "import" clause *)
}

val import_stmt : from_clause option -> import_clause -> import_stmt
(** [import_stmt from import] constructs an import statement where [from] is the
    optional from clause and [import] is the import clause. *)

val deloc_import_stmt : import_stmt -> import_stmt
(** [deloc_import_stmt import_stmt] strips all location information from
    [import_stmt], replacing it with {!Loc.dummy}. *)

(**
 {2 Source File}
 *)

(** A source file *)
type file = private {
  package_stmt: package_stmt;    (** Package name *)
  import_stmts: import_stmt list (** File imports *)
}

val file : package_stmt -> import_stmt list -> file
(** [file pkg imports] constructs a source file where [pkg] is the package
    statement and [imports] is the list of import statements. *)

val deloc_file : file -> file
(** [deloc_file file] strips all location information from [file], replacing it
    with {!Loc.dummy}. *)
