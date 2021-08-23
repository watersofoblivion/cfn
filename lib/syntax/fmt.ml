open Format

open Common

(* Package Statment *)

let package_stmt fmt = function
  | Ast.Package pkg -> fprintf fmt "package %a" Sym.pp_id pkg.id

(* Imports *)

let from_clause fmt = function
  | Ast.From from -> fprintf fmt "@[from@ \"%a\"@ @]" Sym.pp_id from.path

let package_alias fmt = function
  | Ast.Alias alias -> fprintf fmt "@[@ ->@ %a@]" Sym.pp_id alias.alias

let package_clause fmt = function
  | Ast.PackageClause pkg ->
    fprintf fmt "@[\"%a\"" Sym.pp_id pkg.pkg;
    let _ =
      match pkg.alias with
        | Some alias -> package_alias alias fmt
        | None -> ()
    in
    fprintf fmt "@]"

let import_clause fmt = function
  | Ast.Import import ->
    fprintf fmt "@[import";
    let _ =
      match import.pkgs with
        | [] -> ()
        | [package] ->
          fprintf fmt "@ ";
          package_clause package fmt
        | packages ->
          let pp_sep fmt _ = fprintf fmt "@.  | " in
          pp_sep fmt ();
          pp_print_list ~pp_sep package_clause fmt packages
    in
    fprintf fmt "@]"

let import_stmt fmt = function
  | Ast.ImportStmt stmt ->
    fprintf fmt "@[";
    let _ = match stmt.from with
      | Some from -> from_clause from fmt
      | None -> ()
    in
    import_clause stmt.import fmt;
    fprintf fmt "@]"

(* Files *)

let file fmt = function
  | Ast.File f ->
    fprintf fmt "@[%t" package_stmt f.pkg;
    let _ = match f.imports with
      | [] -> ()
      | import_stmts ->
        fprintf fmt "@.@.@[";
        let pp_sep fmt _ = fprintf fmt "@." in
        pp_print_list ~pp_sep import_stmt fmt import_stmts;
        fprintf fmt "@]";
    in
    fprintf fmt "@]"
