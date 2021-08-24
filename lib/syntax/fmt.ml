open Format

open Common

(* Imports *)

let name fmt = function
  | Ast.Name name -> fprintf fmt "%a" Sym.pp_id name.id

let src fmt = function
  | Ast.Source src -> fprintf fmt "%a" name src.name

let from fmt = function
  | Ast.From from -> fprintf fmt "@[from@ \"%a\"@ @]" src from.src

let alias fmt = function
  | Ast.Alias alias ->
    fprintf fmt "@[\"%a\"" name alias.pkg;
    let _ =
      match alias.alias with
        | Some alias -> fprintf fmt "@[@ ->@ %a@]" name alias
        | None -> ()
    in
    fprintf fmt "@]"

let pkgs fmt = function
  | Ast.Packages pkgs ->
    fprintf fmt "@[<v>";
    let pp_sep fmt _ = fprintf fmt "@ | " in
    pp_sep fmt ();
    pp_print_list ~pp_sep alias fmt pkgs.pkgs;
    fprintf fmt "@]"

let import fmt = function
  | Ast.Import stmt ->
    fprintf fmt "@[";
    let _ = match stmt.from with
      | Some clause -> from fmt clause
      | None -> ()
    in
    pkgs fmt stmt.pkgs;
    fprintf fmt "@]"

(* Package Statment *)

let pkg fmt = function
  | Ast.Package pkg -> fprintf fmt "package %a" name pkg.id

(* Files *)

let file fmt = function
  | Ast.File file ->
    fprintf fmt "@[<v>%a" pkg file.pkg;
    let _ = match file.imports with
      | [] -> ()
      | _ ->
        fprintf fmt "@ @ ";
        let pp_sep fmt _ = fprintf fmt "@ " in
        pp_print_list ~pp_sep import fmt file.imports;
    in
    fprintf fmt "@]"
