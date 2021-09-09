%{
  [@@@coverage exclude_file]
  open Common

  let make_name (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Syntax.name loc sym
        |> kontinue env)

  let make_src (start_loc, end_loc) name env kontinue =
    let loc = Loc.loc start_loc end_loc in
    name env (fun env name ->
      Syntax.src loc name
        |> kontinue env)

  let make_from (start_loc, end_loc) src env kontinue =
    let loc = Loc.loc start_loc end_loc in
    src env (fun env src ->
      Syntax.from loc src
        |> kontinue env)

  let make_alias (start_loc, end_loc) pkg alias env kontinue =
    let loc = Loc.loc start_loc end_loc in
    pkg env (fun env pkg ->
      match alias with
        | Some alias ->
          alias env (fun env alias ->
            let alias = Some alias in
            Syntax.alias loc pkg alias
              |> kontinue env)
        | None ->
          Syntax.alias loc pkg None
            |> kontinue env)

  let make_pkgs (start_loc, end_loc) pkgs env kontinue =
    let rec make_pkgs pkgs env kontinue = match pkgs with
        | [] -> kontinue env []
        | pkg :: pkgs ->
          pkg env (fun env pkg ->
            make_pkgs pkgs env (fun env pkgs ->
              pkg :: pkgs
                |> kontinue env))
    in
    let loc = Loc.loc start_loc end_loc in
    make_pkgs pkgs env (fun env pkgs ->
      Syntax.pkgs loc pkgs
        |> kontinue env)

  let make_import (start_loc, end_loc) from pkgs env kontinue =
    let loc = Loc.loc start_loc end_loc in
    pkgs env (fun env pkgs ->
      match from with
        | Some from ->
          from env (fun env from ->
            let from = Some from in
            Syntax.import loc from pkgs
              |> kontinue env)
        | None ->
          Syntax.import loc None pkgs
            |> kontinue env)

  let make_pkg (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    id env (fun env id ->
      Syntax.pkg loc id
        |> kontinue env)
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.name -> 'a) -> 'a> name_test
%start name_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.pkg -> 'a) -> 'a> pkg_test
%start pkg_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.import -> 'a) -> 'a> import_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.from -> 'a) -> 'a>   from_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.src -> 'a) -> 'a>    src_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.pkgs -> 'a) -> 'a>   pkgs_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.alias -> 'a) -> 'a>  alias_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.name -> 'a) -> 'a>   local_test
%start import_test
%start from_test
%start src_test
%start pkgs_test
%start alias_test
%start local_test

%%

/* Test Entry Points */

name_test:
| name = name; EOF { name }

pkg_test:
| pkg = pkg; EOF { pkg }

import_test:
| import = import; EOF { import }

from_test:
| from = from; EOF { from }

src_test:
| src = src; EOF { src }

pkgs_test:
| pkgs = pkgs; EOF { pkgs }

alias_test:
| alias = alias; EOF { alias }

local_test:
| local = local; EOF { local }

/* Common */

%public name:
| id = LIDENT { make_name $sloc id }

/* Package Statement */

%public pkg:
| "package"; id = name { make_pkg $sloc id }

/* Imports */

%public import:
| from = from?; pkgs = pkgs { make_import $sloc from pkgs }

%public from:
| "from"; src = src { make_from $sloc src }

%public src:
| src = name { make_src $sloc src }

%public pkgs:
| "import"; "|"?; pkgs = separated_nonempty_list("|", alias) { make_pkgs $sloc pkgs }

%public alias:
| pkg = name; alias = local? { make_alias $sloc pkg alias }

%public local:
| "->"; local = name { local }
