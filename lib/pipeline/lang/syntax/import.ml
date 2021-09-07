(* Source Files *)

open Format

open Common

(* Syntax *)

type name = Name of { loc: Loc.t; id: Sym.t }

type src = Source of { loc: Loc.t; name: name }
type from = From of { loc: Loc.t; src: src }
type alias = Alias of { loc: Loc.t; pkg: name; alias: name option }
type pkgs = Packages of { loc: Loc.t; pkgs: alias list }
type import = Import of { loc: Loc.t; from: from option; pkgs: pkgs }

type pkg = Package of { loc: Loc.t; id: name }

(* Constructors *)

let name loc id = Name { loc; id }

let src loc name = Source { loc; name }
let from loc src = From { loc; src }
let alias loc pkg alias = Alias { loc; pkg; alias }
let pkgs loc pkgs = Packages { loc; pkgs }
let import loc from pkgs = Import { loc; from; pkgs }

let pkg loc id = Package { loc; id }

(* Location *)

let loc_name = function
  | Name name -> name.loc

let loc_src = function
  | Source src -> src.loc

let loc_from = function
  | From from -> from.loc

let loc_alias = function
  | Alias alias -> alias.loc

let loc_pkgs = function
  | Packages pkgs -> pkgs.loc

let loc_import = function
  | Import import -> import.loc

let loc_pkg = function
  | Package pkg -> pkg.loc

(* Pretty Printing *)

let pp_name fmt = function
  | Name name -> fprintf fmt "%a" Sym.pp_id name.id

let pp_src fmt = function
  | Source src -> fprintf fmt "%a" pp_name src.name

let pp_from fmt = function
  | From from -> fprintf fmt "@[from@ %a@ @]" pp_src from.src

let pp_alias fmt = function
  | Alias alias ->
    fprintf fmt "@[%a" pp_name alias.pkg;
    let _ =
      match alias.alias with
        | Some alias -> fprintf fmt "@[@ ->@ %a@]" pp_name alias
        | None -> ()
    in
    fprintf fmt "@]"

let pp_pkgs fmt = function
  | Packages pkgs ->
    fprintf fmt "@[<v>";
    let pp_sep fmt _ = fprintf fmt "@ | " in
    pp_sep fmt ();
    pp_print_list ~pp_sep pp_alias fmt pkgs.pkgs;
    fprintf fmt "@]"

let pp_import fmt = function
  | Import stmt ->
    fprintf fmt "@[";
    let _ = match stmt.from with
      | Some clause -> pp_from fmt clause
      | None -> ()
    in
    fprintf fmt "import";
    pp_pkgs fmt stmt.pkgs;
    fprintf fmt "@]"

(* Package Statment *)

let pp_pkg fmt = function
  | Package pkg -> fprintf fmt "package %a" pp_name pkg.id
