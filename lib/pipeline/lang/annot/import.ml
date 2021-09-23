(* Imports *)

open Format

open Common

(* Syntax *)

type proto =
  | ProtoHttp
  | ProtoHttps
  | ProtoGit
  | ProtoGitSsh

type host =
  | HostGitHub of { owner: string; repo: string }
  | HostGitLab of { owner: string; repo: string }
  | HostRemote of { proto: proto option; uri: string }

type src =
  | SrcCurrent
  | SrcExternal of { host: host; version: int }

type alias = Alias of { pkg: string; alias: Sym.t }
type import = Import of { from: src option; pkgs: alias list }
type pkg = Package of { name: Sym.t; imports: import list; tops: Top.top list }

(* Constructors *)

let proto_http = ProtoHttp
let proto_https = ProtoHttps
let proto_git = ProtoGit
let proto_git_ssh = ProtoGitSsh

let host_github owner repo = HostGitHub { owner; repo }
let host_gitlab owner repo = HostGitLab { owner; repo }
let host_remote proto uri = HostRemote { proto; uri }

let src_current = SrcCurrent
let src_external host version = SrcExternal { host; version }

let alias pkg alias = Alias { pkg; alias }
let import from pkgs = Import { from; pkgs }
let pkg name imports tops = Package { name; imports; tops }

(* Pretty-Printing *)

let pp_proto fmt = function
  | ProtoHttp -> fprintf fmt "http"
  | ProtoHttps -> fprintf fmt "https"
  | ProtoGit -> fprintf fmt "git"
  | ProtoGitSsh -> fprintf fmt "git+ssh"

let pp_host fmt = function
  | HostGitHub host -> fprintf fmt "github.com/%s/%s" host.owner host.repo
  | HostGitLab host -> fprintf fmt "gitlab.com/%s/%s" host.owner host.repo
  | HostRemote host ->
    let pp_proto = pp_print_option (fun fmt proto -> fprintf fmt "%a://" pp_proto proto) in
    fprintf fmt "%a%s" pp_proto host.proto host.uri

let pp_src fmt = function
  | SrcCurrent -> fprintf fmt "."
  | SrcExternal src -> fprintf fmt "%a%@v%d" pp_host src.host src.version

let pp_alias fmt = function
  | Alias alias ->
    fprintf fmt "%s -> %a" alias.pkg Sym.pp alias.alias

let pp_import fmt = function
  | Import import ->
    let pp_from = pp_print_option (fun fmt src -> fprintf fmt "from %a " pp_src src) in
    pp_open_vbox fmt 0;
    fprintf fmt "%aimport" pp_from import.from;
    pp_print_space fmt ();
      pp_open_vbox fmt 2;
      let pp_sep fmt _ =
        pp_print_space fmt ();
        fprintf fmt "| "
      in
      fprintf fmt "| %a" (pp_print_list ~pp_sep pp_alias) import.pkgs;
      pp_close_box fmt ();
    pp_close_box fmt ()

let pp_pkg fmt pkg =
  let pp_prefix fmt =
    pp_print_space fmt ();
    pp_print_space fmt ()
  in
  let pp_list pp = function
    | [] -> ()
    | lst -> fprintf fmt "%t%a" pp_prefix (pp_print_list ~pp_sep:pp_print_space pp) lst
  in
  match pkg with
    | Package pkg ->
      pp_open_vbox fmt 0;
      fprintf fmt "package %a" Sym.pp pkg.name;
      pp_list pp_import pkg.imports;
      pp_list Top.pp_top pkg.tops;
      pp_close_box fmt ()
