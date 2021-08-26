%{
  open Common

  let make_ty_constr (start_loc, end_loc) constr env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.constr_of constr env (fun env sym ->
      Type.constr loc sym
        |> kontinue env)

  let make_lit_bool (start_loc, end_loc) b env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.bool loc b
      |> kontinue env

  let make_lit_int (start_loc, end_loc) i env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.int loc i
      |> kontinue env

  let make_lit_long (start_loc, end_loc) l env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.long loc l
      |> kontinue env

  let make_lit_float (start_loc, end_loc) f env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.float loc f
      |> kontinue env

  let make_lit_double (start_loc, end_loc) d env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.double loc d
      |> kontinue env

  let make_lit_rune (start_loc, end_loc) r env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.rune loc r
      |> kontinue env

  let make_lit_string (start_loc, end_loc) s env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Ast.string loc s
      |> kontinue env

  let make_ident (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Ast.ident loc sym
        |> kontinue env)

  let make_patt_ground (start_loc, end_loc) env kontinue =
    Loc.loc start_loc end_loc
      |> Ast.patt_ground
      |> kontinue env

  let make_patt_var (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Ast.patt_var loc sym
        |> kontinue env)

  let make_value_binding (start_loc, end_loc) patt ty value env kontinue =
    let loc = Loc.loc start_loc end_loc in
    value env (fun _ value ->
      patt env (fun env patt ->
        match ty with
          | None ->
            Ast.value_binding loc patt None value
              |> kontinue env
          | Some ty ->
            ty env (fun env ty ->
              Ast.value_binding loc patt (Some ty) value
                |> kontinue env)))

  let make_top_let (start_loc, end_loc) binding env kontinue =
    let loc = Loc.loc start_loc end_loc in
    binding env (fun env binding ->
      Ast.top_let loc binding
        |> kontinue env)

  let make_top_val (start_loc, end_loc) binding env kontinue =
    let loc = Loc.loc start_loc end_loc in
    binding env (fun env binding ->
      Ast.top_val loc binding
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
      Ast.pkg loc id
        |> kontinue env)

  let make_file pkg imports tops env kontinue =
    pkg env (fun env pkg ->
      make_imports imports env (fun env imports ->
        make_tops tops env (fun env tops ->
          Ast.file pkg imports tops
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

/* Keywords */
%token PACKAGE "package"
%token FROM "from"
%token IMPORT "import"
%token LET "let"
%token VAL "val"

/* Literals */
%token <bool> BOOL
%token <string> INT LONG FLOAT DOUBLE
%token <Uchar.t> RUNE
%token <Uchar.t list> STRING

/* Identifiers */
%token <string> UIDENT LIDENT

/* Main Entry Points */
%type <'a Env.t -> ('a Env.t -> Ast.file -> 'b) -> 'b> package_only imports_only file

%start package_only
%start imports_only
%start file

/* Testing Entry Points */
%type <'a Env.t -> ('a Env.t -> Ast.pkg -> 'b) -> 'b>     pkg_test
%type <'a Env.t -> ('a Env.t -> Ast.import -> 'b) -> 'b>  import_test
%type <'a Env.t -> ('a Env.t -> Ast.top -> 'b) -> 'b>     top_test
%type <'a Env.t -> ('a Env.t -> Ast.binding -> 'b) -> 'b> binding_test
%type <'a Env.t -> ('a Env.t -> Ast.patt -> 'b) -> 'b>    patt_test
%type <'a Env.t -> ('a Env.t -> Ast.expr -> 'b) -> 'b>    expr_test
%type <'a Env.t -> ('a Env.t -> Type.t -> 'b) -> 'b>      ty_test

%start pkg_test
%start import_test
%start top_test
%start binding_test
%start patt_test
%start expr_test
%start ty_test

%%

/* Test Entry Points */

pkg_test:
| pkg = pkg; EOF { pkg }

import_test:
| import = import; EOF { import }

top_test:
| top = top; EOF { top }

binding_test:
| binding = binding; EOF { binding }

patt_test:
| patt = patt; EOF { patt }

expr_test:
| expr = expr; EOF { expr }

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
| id = ident { make_ident $sloc id }

ident:
| id = UIDENT { id }
| id = LIDENT { id }

lit:
| b = BOOL   { make_lit_bool $sloc b }
| i = INT    { make_lit_int $sloc i }
| l = LONG   { make_lit_long $sloc l }
| f = FLOAT  { make_lit_float $sloc f }
| d = DOUBLE { make_lit_double $sloc d }
| r = RUNE   { make_lit_rune $sloc r }
| s = STRING { make_lit_string $sloc s }

/* Types */

annot:
| ":"; ty = ty { ty }

ty:
| constr = UIDENT { make_ty_constr $sloc constr }
