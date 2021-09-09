%{
  [@@@coverage exclude_file]
  open Common

  let make_ty_constr (start_loc, end_loc) constr env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.constr_of constr env (fun env sym ->
      Syntax.ty_constr loc sym
        |> kontinue env)

  let make_rune_lit (start_loc, end_loc) uchar env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.rune_lit loc uchar
      |> kontinue env

  let make_rune_escape (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.rune_escape loc lexeme
      |> kontinue env

  let make_str_lit (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.str_lit loc lexeme
      |> kontinue env

  let make_str_escape (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.str_escape loc lexeme
      |> kontinue env

  let make_lit_bool (start_loc, end_loc) b env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.expr_bool loc b
      |> kontinue env

  let make_lit_int (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.expr_int loc lexeme
      |> kontinue env

  let make_lit_long (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.expr_long loc lexeme
      |> kontinue env

  let make_lit_float (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.expr_float loc lexeme
      |> kontinue env

  let make_lit_double (start_loc, end_loc) lexeme env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Syntax.expr_double loc lexeme
      |> kontinue env

  let make_lit_rune (start_loc, end_loc) r env kontinue =
    let loc = Loc.loc start_loc end_loc in
    r env (fun env r ->
      Syntax.expr_rune loc r
        |> kontinue env)

  let rec make_str_segs segs env kontinue = match segs with
    | [] -> kontinue env []
    | seg :: segs ->
      seg env (fun env seg ->
        make_str_segs segs env (fun env segs ->
          seg :: segs
            |> kontinue env))

  let make_lit_string (start_loc, end_loc) segs env kontinue =
    let loc = Loc.loc start_loc end_loc in
    make_str_segs segs env (fun env segs ->
      Syntax.expr_string loc segs
        |> kontinue env)

  let make_ident (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Syntax.expr_ident loc sym
        |> kontinue env)

  let make_patt_ground (start_loc, end_loc) env kontinue =
    Loc.loc start_loc end_loc
      |> Syntax.patt_ground
      |> kontinue env

  let make_patt_var (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Syntax.patt_var loc sym
        |> kontinue env)

  let make_value_binding (start_loc, end_loc) patt ty value env kontinue =
    let loc = Loc.loc start_loc end_loc in
    value env (fun _ value ->
      patt env (fun env patt ->
        match ty with
          | None ->
            Syntax.value_binding loc patt None value
              |> kontinue env
          | Some ty ->
            ty env (fun env ty ->
              Syntax.value_binding loc patt (Some ty) value
                |> kontinue env)))

  let make_top_let (start_loc, end_loc) binding env kontinue =
    let loc = Loc.loc start_loc end_loc in
    binding env (fun env binding ->
      Syntax.top_let loc binding
        |> kontinue env)

  let make_top_val (start_loc, end_loc) binding env kontinue =
    let loc = Loc.loc start_loc end_loc in
    binding env (fun env binding ->
      Syntax.top_val loc binding
        |> kontinue env)

  let rec make_tops tops env kontinue = match tops with
    | [] -> kontinue env []
    | top :: tops ->
      top env (fun env top ->
        make_tops tops env (fun env tops ->
          top :: tops
            |> kontinue env))

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

  let rec make_imports imports env kontinue = match imports with
    | [] -> kontinue env []
    | import :: imports ->
      import env (fun env import ->
        make_imports imports env (fun env imports ->
          import :: imports
            |> kontinue env))

  let make_pkg (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    id env (fun env id ->
      Syntax.pkg loc id
        |> kontinue env)

  let make_file pkg imports tops env kontinue =
    pkg env (fun env pkg ->
      make_imports imports env (fun env imports ->
        make_tops tops env (fun env tops ->
          Syntax.file pkg imports tops
            |> kontinue env)))
%}

/* Non-Printable */
%token EOF

/* Punctuation */
%token PIPE "|"
%token ARROW "->"
%token COLON ":"
%token BIND "="
%token GROUND "_"
%token SQUOTE "'"
%token DQUOTE

/* Keywords */
%token PACKAGE "package"
%token FROM "from"
%token IMPORT "import"
%token LET "let"
%token VAL "val"

/* Literals */
%token <bool> BOOL
%token <string> INT LONG FLOAT DOUBLE

/* Unicode */
%token <string> UESC

/* Runes */
%token <Uchar.t> RUNE

/* Strings */
%token <string> STRING

/* Identifiers */
%token <string> UIDENT LIDENT

/* Main Entry Points */
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.file -> 'a) -> 'a> package_only imports_only file

%start package_only
%start imports_only
%start file

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

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.top -> 'a) -> 'a> top_test
%start top_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.binding -> 'a) -> 'a> binding_test
%start binding_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.patt -> 'a) -> 'a> patt_test
%start patt_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> expr_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> ident_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> lit_test
%start expr_test
%start ident_test
%start lit_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.ty -> 'a) -> 'a> annot_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.ty -> 'a) -> 'a> ty_test
%start annot_test
%start ty_test

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

top_test:
| top = top; EOF { top }

binding_test:
| binding = binding; EOF { binding }

patt_test:
| patt = patt; EOF { patt }

expr_test:
| expr = expr; EOF { expr }

ident_test:
| ident = ident; EOF { ident }

lit_test:
| lit = lit; EOF { lit }

annot_test:
| annot = annot; EOF { annot }

ty_test:
| ty = ty; EOF { ty }

/* Source Files */

package_only:
| pkg = pkg; end_of_package_only { make_file pkg [] [] }

end_of_package_only:
| "from"              { () }
| "import"            { () }
| end_of_imports_only { () }

imports_only:
| pkg = pkg; imports = list(import); end_of_imports_only { make_file pkg imports [] }

end_of_imports_only:
| "let"       { () }
| "val"       { () }
| end_of_file { () }

file:
| pkg = pkg; imports = list(import); tops = list(top); end_of_file { make_file pkg imports tops }

end_of_file:
| EOF { () }

/* Common */

name:
| id = LIDENT { make_name $sloc id }

/* Package Statement */

pkg:
| "package"; id = name { make_pkg $sloc id }

/* Imports */

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

/* Top-Level Expressions */

top:
| "let"; binding = binding { make_top_let $sloc binding }
| "val"; binding = binding { make_top_val $sloc binding }

/* Bindings */

binding:
| patt = patt; ty = annot?; BIND; value = expr { make_value_binding $sloc patt ty value }

/* Patterns */

patt:
| "_"         { make_patt_ground $sloc }
| id = LIDENT { make_patt_var $sloc id }

/* Expressions */

expr:
| lit = lit  { lit }
| id = ident { id }

ident:
| id = UIDENT { make_ident $sloc id }
| id = LIDENT { make_ident $sloc id }

lit:
| b = BOOL                             { make_lit_bool $sloc b }
| i = INT                              { make_lit_int $sloc i }
| l = LONG                             { make_lit_long $sloc l }
| f = FLOAT                            { make_lit_float $sloc f }
| d = DOUBLE                           { make_lit_double $sloc d }
| "'"; r = rune; "'"                   { make_lit_rune $sloc r }
| DQUOTE; segs = list(str_seg); DQUOTE { make_lit_string $sloc segs }

rune:
| r = RUNE    { make_rune_lit $sloc r }
| uesc = UESC { make_rune_escape $sloc uesc }

str_seg:
| s = STRING  { make_str_lit $sloc s }
| uesc = UESC { make_str_escape $sloc uesc }

/* Types */

annot:
| ":"; ty = ty { ty }

ty:
| constr = UIDENT { make_ty_constr $sloc constr }
