%{
  open Common

  let make_name (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Ast.name loc sym
        |> kontinue env)

  let make_src (start_loc, end_loc) name env kontinue =
    let loc = Loc.loc start_loc end_loc in
    name env (fun env name ->
      Ast.src loc name
        |> kontinue env)

  let make_from (start_loc, end_loc) src env kontinue =
    let loc = Loc.loc start_loc end_loc in
    src env (fun env src ->
      Ast.from loc src
        |> kontinue env)

  let make_alias (start_loc, end_loc) pkg alias env kontinue =
    let loc = Loc.loc start_loc end_loc in
    pkg env (fun env pkg ->
      match alias with
        | Some alias ->
          alias env (fun env alias ->
            let alias = Some alias in
            Ast.alias loc pkg alias
              |> kontinue env)
        | None ->
          Ast.alias loc pkg None
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
      Ast.pkgs loc pkgs
        |> kontinue env)

  let make_import (start_loc, end_loc) from pkgs env kontinue =
    let loc = Loc.loc start_loc end_loc in
    pkgs env (fun env pkgs ->
      match from with
        | Some from ->
          from env (fun env from ->
            let from = Some from in
            Ast.import loc from pkgs
              |> kontinue env)
        | None ->
          Ast.import loc None pkgs
            |> kontinue env)

  let make_pkg (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    id env (fun env id ->
      Ast.pkg loc id
        |> kontinue env)

  let make_file pkg imports env kontinue =
    let rec make_imports imports env kontinue = match imports with
      | [] -> kontinue env []
      | import :: imports ->
        import env (fun env import ->
          make_imports imports env (fun env imports ->
            import :: imports
              |> kontinue env))
    in
    pkg env (fun env pkg ->
      make_imports imports env (fun env imports ->
        Ast.file pkg imports
          |> kontinue env))
%}

%token EOF
%token PACKAGE "package"
%token FROM "from"
%token IMPORT "import"
%token PIPE "|"
%token ARROW "->"

%token <bool> BOOL
%token <string> INT LONG FLOAT DOUBLE
%token <Uchar.t> RUNE
%token <Uchar.t list> STRING
%token <string> LIDENT

/* Main Entry Points */
%type <'a Env.t -> ('a Env.t -> Ast.file -> 'b) -> 'b> package_only imports_only file

%start package_only
%start imports_only
%start file

/* Testing Entry Points */
%type <'a Env.t -> ('a Env.t -> Ast.pkg -> 'b) -> 'b>    pkg_test
%type <'a Env.t -> ('a Env.t -> Ast.import -> 'b) -> 'b> import_test
%type <unit> lit_test

%start pkg_test
%start import_test
%start lit_test

%%

/*
 * Test Entry Points
 */

pkg_test:
| pkg = pkg; EOF { pkg }

import_test:
| import = import; EOF { import }

lit_test:
| lit = lit; EOF { lit }

/*
 * Source Files
 */

package_only:
| pkg = pkg; end_of_package_only { make_file pkg [] }

end_of_package_only:
| "from"              { () }
| "import"            { () }
| end_of_imports_only { () }

imports_only:
| pkg = pkg; imports = list(import); end_of_imports_only { make_file pkg imports }

end_of_imports_only:
| end_of_file { () }

file:
| pkg = pkg; imports = list(import); end_of_file { make_file pkg imports }

end_of_file:
| EOF { () }

/*
 * Common
 */

name:
| id = LIDENT { make_name $sloc id }

/*
 * Package Statement
 */

pkg:
| "package"; id = name { make_pkg $sloc id }

/*
 * Imports
 */

import:
| from = from?; pkgs = pkgs { make_import $sloc from pkgs }

from:
| "from"; src = src { make_from $sloc src }

src:
| src = name { make_src $sloc src }

pkgs:
| "import"; "|"?; pkgs = separated_nonempty_list("|", alias) { make_pkgs $sloc pkgs }

alias:
| pkg = name; alias = local? { make_alias $sloc pkg alias }

local:
| "->"; local = name { local }

/*
 * Expressions
 */

lit:
| BOOL   { () }
| INT    { () }
| LONG   { () }
| FLOAT  { () }
| DOUBLE { () }
| RUNE   { () }
| STRING { () }
