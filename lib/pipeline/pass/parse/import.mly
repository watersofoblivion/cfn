%{
  [@@@coverage exclude_file]
  open Common

  let make_name (start_pos, end_pos) id env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Env.rename id env (fun env sym ->
      Syntax.name loc sym
        |> kontinue env)

  let make_proto (start_pos, end_pos) proto env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Syntax.proto loc proto
      |> kontinue env

  let make_host (start_pos, end_pos) host env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Syntax.host loc host
      |> kontinue env

  let make_hostpath (start_pos, end_pos) segs env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Syntax.hostpath loc segs
      |> kontinue env

  let make_version (start_pos, end_pos) version env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Syntax.version loc version
      |> kontinue env

  let make_src_current (start_pos, end_pos) env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Syntax.src_current loc
      |> kontinue env

  let make_src_external (start_pos, end_pos) proto host path version env kontinue =
    let loc = Loc.loc start_pos end_pos in
    make_optional proto env (fun env proto ->
      host env (fun env host ->
        make_optional path env (fun env path ->
          version env (fun env version ->
            Syntax.src_external loc proto host path version
              |> kontinue env))))

  let make_from (start_pos, end_pos) src env kontinue =
    let loc = Loc.loc start_pos end_pos in
    src env (fun env src ->
      Syntax.from loc src
        |> kontinue env)

  let make_pkgpath (start_pos, end_pos) segs env kontinue =
    let rec make_segs segs env kontinue = match segs with
      | [] -> kontinue env []
      | seg :: segs ->
        seg env (fun env seg ->
          make_segs segs env (fun env segs ->
            seg :: segs
              |> kontinue env))
    in
    let loc = Loc.loc start_pos end_pos in
    make_segs segs env (fun env segs ->
      Syntax.pkgpath loc segs
        |> kontinue env)

  let make_alias (start_pos, end_pos) pkg alias env kontinue =
    let loc = Loc.loc start_pos end_pos in
    pkg env (fun env pkg ->
      make_optional alias env (fun env alias ->
        Syntax.alias loc pkg alias
          |> kontinue env))

  let make_pkgs (start_pos, end_pos) pkgs env kontinue =
    let rec make_pkgs pkgs env kontinue = match pkgs with
        | [] -> kontinue env []
        | pkg :: pkgs ->
          pkg env (fun env pkg ->
            make_pkgs pkgs env (fun env pkgs ->
              pkg :: pkgs
                |> kontinue env))
    in
    let loc = Loc.loc start_pos end_pos in
    make_pkgs pkgs env (fun env pkgs ->
      Syntax.pkgs loc pkgs
        |> kontinue env)

  let make_import (start_pos, end_pos) from pkgs env kontinue =
    let loc = Loc.loc start_pos end_pos in
    pkgs env (fun env pkgs ->
      make_optional from env (fun env from ->
        Syntax.import loc from pkgs
          |> kontinue env))
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.name -> 'a) -> 'a> name_test
%start name_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.import -> 'a) -> 'a>  import_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.from -> 'a) -> 'a>    from_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.src -> 'a) -> 'a>     src_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.pkgs -> 'a) -> 'a>    pkgs_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.alias -> 'a) -> 'a>   alias_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.pkgpath -> 'a) -> 'a> pkgpath_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.name -> 'a) -> 'a>    local_test
%start import_test
%start from_test
%start src_test
%start pkgs_test
%start alias_test
%start pkgpath_test
%start local_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.proto -> 'a) -> 'a>    proto_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.host -> 'a) -> 'a>     host_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.hostpath -> 'a) -> 'a> hostpath_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.version -> 'a) -> 'a>  version_test
%start proto_test
%start host_test
%start hostpath_test
%start version_test

%%

/* Test Entry Points */

name_test:
| name = name; EOF { name }

import_test:
| import = import; EOF { import }

from_test:
| from = from; EOF { from }

src_test:
| src = src; EOF { src }

proto_test:
| proto = proto; EOF { proto }

host_test:
| host = host; EOF { host }

hostpath_test:
| hostpath = hostpath; EOF { hostpath }

version_test:
| version = version; EOF { version }

pkgs_test:
| pkgs = pkgs; EOF { pkgs }

alias_test:
| alias = alias; EOF { alias }

pkgpath_test:
| pkgpath = pkgpath; EOF { pkgpath }

local_test:
| local = local; EOF { local }

/* Common */

%public name:
| id = LIDENT { make_name $sloc id }

/* Imports */

%public import:
| from = from?; pkgs = pkgs { make_import $sloc from pkgs }

%public from:
| "from"; WHITESPACE; src = src; WHITESPACE { make_from ($startpos, $endpos(src)) src }

%public src:
| "."                                                              { make_src_current $sloc }
| proto = proto?; host = host; path = hostpath?; version = version { make_src_external $sloc proto host path version }

%inline %public proto:
| proto = PATH_SEG; "://" { make_proto $sloc proto }

%inline %public host:
| host = HOST { make_host $sloc host }

%inline %public hostpath:
| PATH_SEP; segs = separated_list(PATH_SEP, PATH_SEG) { make_hostpath $sloc segs }

%inline %public version:
| "@"; "v"; version = VERSION { make_version $sloc version }

%public pkgs:
| "import"; "|"?; pkgs = separated_nonempty_list("|", alias) { make_pkgs $sloc pkgs }

%public alias:
| pkg = pkgpath; alias = local? { make_alias $sloc pkg alias }

%public %inline pkgpath:
| DQUOTE; segs = list(str); DQUOTE { make_pkgpath $sloc segs }

%public %inline local:
| "->"; local = name { local }
