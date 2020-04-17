open Format

(* Package Statment *)

let package_stmt fmt stmt =
  fprintf fmt "package %s" stmt.Ast.pkg_name

(* Imports *)

let from_clause fmt from =
  fprintf fmt "@[from@ %S@ @]" from.Ast.import_path

let package_alias fmt alias =
  fprintf fmt "@[@ ->@ %s@]" alias.Ast.local_alias

let package_clause fmt pkg =
  fprintf fmt "@[%S" pkg.Ast.package_path;
  let _ =
    match pkg.alias with
      | Some alias -> package_alias fmt alias
      | None -> ()
  in
  fprintf fmt "@]"

let import_clause fmt import =
  fprintf fmt "@[import";
  let _ =
    match import.Ast.packages with
      | [] -> ()
      | [package] ->
        fprintf fmt "@ ";
        package_clause fmt package
      | packages ->
        let pp_sep fmt _ = fprintf fmt "@.  | " in
        pp_sep fmt ();
        pp_print_list ~pp_sep package_clause fmt packages
  in
  fprintf fmt "@]"

let import_stmt fmt stmt =
  fprintf fmt "@[";
  let _ = match stmt.Ast.from with
    | Some from -> from_clause fmt from
    | None -> ()
  in
  import_clause fmt stmt.import;
  fprintf fmt "@]"

(* Files *)

let file fmt f =
  fprintf fmt "@[%a" package_stmt f.Ast.package_stmt;
  let _ = match f.import_stmts with
    | [] -> ()
    | import_stmts ->
      fprintf fmt "@.@.@[";
      let pp_sep fmt _ = fprintf fmt "@." in
      pp_print_list ~pp_sep import_stmt fmt import_stmts;
      fprintf fmt "@]";
  in
  fprintf fmt "@]"
