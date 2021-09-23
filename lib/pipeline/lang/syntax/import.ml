(* Source Files *)

open Format

open Common

(* Syntax *)

type name = Name of { loc: Loc.t; id: Sym.t }

type proto = Proto of { loc: Loc.t; proto: string }
type host = Host of { loc: Loc.t; host: string }
type hostpath = HostPath of { loc: Loc.t; segs: string list }
type version = Version of { loc: Loc.t; version: string }
type src =
  | Current of { loc: Loc.t }
  | External of { loc: Loc.t; proto: proto option; host: host; path: hostpath option; version: version }
type from = From of { loc: Loc.t; src: src }
type pkgpath = PkgPath of { loc: Loc.t; path: Expr.str list }
type alias = Alias of { loc: Loc.t; pkg: pkgpath; alias: name option }
type pkgs = Packages of { loc: Loc.t; pkgs: alias list }
type import = Import of { loc: Loc.t; from: from option; pkgs: pkgs }

type pkg = Package of { loc: Loc.t; id: name }

(* Constructors *)

let name loc id = Name { loc; id }

let proto loc proto = Proto { loc; proto }
let host loc host = Host { loc; host }
let hostpath loc segs = HostPath { loc; segs }
let version loc version = Version { loc; version }
let src_current loc = Current { loc }
let src_external loc proto host path version = External { loc; proto; host; path; version }
let from loc src = From { loc; src }
let pkgpath loc path = PkgPath { loc; path }
let alias loc pkg alias = Alias { loc; pkg; alias }
let pkgs loc pkgs = Packages { loc; pkgs }
let import loc from pkgs = Import { loc; from; pkgs }

let pkg loc id = Package { loc; id }

(* Location *)

let loc_name = function
  | Name name -> name.loc

let loc_proto = function
  | Proto proto -> proto.loc

let loc_host = function
  | Host host -> host.loc

let loc_hostpath = function
  | HostPath path -> path.loc

let loc_version = function
  | Version version -> version.loc

let loc_src = function
  | Current src -> src.loc
  | External src -> src.loc

let loc_from = function
  | From from -> from.loc

let loc_pkgpath = function
  | PkgPath path -> path.loc

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

let pp_proto fmt = function
  | Proto proto -> fprintf fmt "%s" proto.proto

let pp_host fmt = function
  | Host host -> fprintf fmt "%s" host.host

let pp_hostpath fmt = function
  | HostPath path ->
    let pp fmt str = fprintf fmt "%s" str in
    let pp_sep fmt _ = fprintf fmt "/" in
    fprintf fmt "%a" (pp_print_list ~pp_sep pp) path.segs

let pp_version fmt = function
  | Version version -> fprintf fmt "%s" version.version

let pp_src fmt = function
  | Current _ -> fprintf fmt "."
  | External src ->
    let pp_proto = pp_print_option (fun fmt proto -> fprintf fmt "%a://" pp_proto proto) in
    let pp_hostpath = pp_print_option (fun fmt path -> fprintf fmt "/%a" pp_hostpath path) in
    fprintf fmt "%a%a%a@v%a"
      pp_proto src.proto
      pp_host src.host
      pp_hostpath src.path
      pp_version src.version

let pp_from fmt = function
  | From from -> fprintf fmt "@[from@ %a@ @]" pp_src from.src

let pp_pkgpath fmt = function
  | PkgPath path ->
    let pp_sep _ _ = () in
    fprintf fmt "\"%a\"" (pp_print_list ~pp_sep Expr.pp_str) path.path

let pp_alias fmt = function
  | Alias alias ->
    let pp_alias = pp_print_option (fun fmt alias -> fprintf fmt "@[@ ->@ %a@]" pp_name alias) in
    fprintf fmt "@[%a%a@]" pp_pkgpath alias.pkg pp_alias alias.alias

let pp_pkgs fmt = function
  | Packages pkgs ->
    fprintf fmt "@[<v>";
    let pp_sep fmt _ = fprintf fmt "@ | " in
    pp_sep fmt ();
    pp_print_list ~pp_sep pp_alias fmt pkgs.pkgs;
    fprintf fmt "@]"

let pp_import fmt = function
  | Import import ->
    fprintf fmt "@[";
    let pp_from = pp_print_option pp_from in
    fprintf fmt "%aimport" pp_from import.from;
    pp_pkgs fmt import.pkgs;
    fprintf fmt "@]"

(* Package Statment *)

let pp_pkg fmt = function
  | Package pkg -> fprintf fmt "package %a" pp_name pkg.id
