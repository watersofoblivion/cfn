(* Package statement *)

type package_stmt = {
  package_stmt_loc: Loc.t;
  name_loc:         Loc.t;
  pkg_name:         string
}

let package_stmt kwd_loc name_loc name =
  { package_stmt_loc = Loc.span kwd_loc name_loc;
    name_loc         = name_loc;
    pkg_name         = name }

let deloc_package_stmt package_stmt =
  { package_stmt_loc = Loc.dummy;
    name_loc         = Loc.dummy;
    pkg_name         = package_stmt.pkg_name }

(* Imports *)

type from_clause = {
  from_clause_loc: Loc.t;
  ip_loc:          Loc.t;
  import_path:     string
}

let from_clause kwd_loc ip_loc import_path =
  { from_clause_loc = Loc.span kwd_loc ip_loc;
    ip_loc          = ip_loc;
    import_path     = import_path }

let deloc_from_clause from_clause =
  { from_clause_loc = Loc.dummy;
    ip_loc          = Loc.dummy;
    import_path     = from_clause.import_path }

type package_alias = {
  package_alias_loc: Loc.t;
  local_alias:       string
}

let package_alias loc alias =
  { package_alias_loc = loc;
    local_alias       = alias }

let deloc_package_alias package_alias =
  { package_alias_loc = Loc.dummy;
    local_alias       = package_alias.local_alias }

type package_clause = {
  package_clause_loc: Loc.t;
  pp_loc:             Loc.t;
  package_path:       string;
  alias:              package_alias option
}

let package_clause pp_loc package_path alias =
  let loc = match alias with
    | Some alias -> Loc.span pp_loc alias.package_alias_loc
    | None -> pp_loc
  in
  { package_clause_loc = loc;
    pp_loc             = pp_loc;
    package_path       = package_path;
    alias              = alias }

let deloc_package_clause package_clause =
  let alias = match package_clause.alias with
    | Some alias -> Some (deloc_package_alias alias)
    | None -> None
  in
  { package_clause_loc = Loc.dummy;
    pp_loc             = Loc.dummy;
    package_path       = package_clause.package_path;
    alias              = alias }

type import_clause = {
  import_clause_loc: Loc.t;
  packages:          package_clause list
}

let import_clause kwd_loc packages =
  let loc =
    let rec last_loc = function
      | [] -> kwd_loc
      | [package] -> package.package_clause_loc
      | _::packages -> last_loc packages
    in
    Loc.span kwd_loc (last_loc packages)
  in
  { import_clause_loc = loc;
    packages          = packages }

let deloc_import_clause import_clause =
  { import_clause_loc = Loc.dummy;
    packages          = List.map deloc_package_clause import_clause.packages }

type import_stmt = {
  import_stmt_loc: Loc.t;
  from:            from_clause option;
  import:          import_clause
}

let import_stmt from import =
  let loc =
    match from with
    | Some from -> Loc.span from.from_clause_loc import.import_clause_loc
    | None -> import.import_clause_loc
  in
  { import_stmt_loc = loc;
    from            = from;
    import          = import }

let deloc_import_stmt import_stmt =
  let from = match import_stmt.from with
    | Some from -> Some (deloc_from_clause from)
    | None -> None
  in
  { import_stmt_loc = Loc.dummy;
    from            = from;
    import          = deloc_import_clause import_stmt.import }

(* Source Files *)

type file = {
  package_stmt: package_stmt;
  import_stmts: import_stmt list
}

let file pkg imports =
  { package_stmt = pkg;
    import_stmts = imports }

let deloc_file file =
  { package_stmt = deloc_package_stmt file.package_stmt;
    import_stmts = List.map deloc_import_stmt file.import_stmts }
