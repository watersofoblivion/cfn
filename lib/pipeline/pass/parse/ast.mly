%{
  [@@@coverage exclude_file]
  open Common

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

  let rec make_str_rest_segs line env kontinue = match line with
    | [] -> kontinue env []
    | seg :: segs ->
      seg env (fun env seg ->
        make_str_rest_segs segs env (fun env segs ->
          seg :: segs
            |> kontinue env))

  let whitespace_re = Str.regexp "^[ \t]+"
  let make_str_segs col line env kontinue =
    let strip_prefix col lit =
      let lit_len = String.length lit in
      let prefix_len = min col lit_len in
      let suffix_len = lit_len - prefix_len in
      let suffix = String.sub lit prefix_len suffix_len in
      let prefix =
        String.sub lit 0 prefix_len
          |> Str.replace_first whitespace_re ""
      in
      prefix ^ suffix
    in
    let on_lit fn = function
      | Syntax.StringLit lit -> Syntax.str_lit lit.loc (fn lit.lexeme)
      | seg -> seg
    in
    match line with
      | [] -> kontinue env []
      | seg :: segs ->
        seg env (fun env seg ->
          let seg = on_lit (strip_prefix col) seg in
          make_str_rest_segs segs env (fun env segs ->
            seg :: segs
              |> kontinue env))

  let rec make_str_rest_lines col lines env kontinue = match lines with
    | [] -> kontinue env []
    | line :: lines ->
      make_str_segs col line env (fun env line ->
        make_str_rest_lines col lines env (fun env lines ->
          line :: lines
            |> kontinue env))

  let make_str_lines col lines env kontinue = match lines with
    | [] -> kontinue env []
    | line :: lines ->
      make_str_rest_segs line env (fun env line ->
        make_str_rest_lines col lines env (fun env lines ->
          line :: lines
            |> kontinue env))

  let make_lit_string (start_loc, end_loc) lines env kontinue =
    let loc = Loc.loc start_loc end_loc in
    let col = start_loc.Lexing.pos_cnum - start_loc.Lexing.pos_bol + 1 in
    make_str_lines col lines env (fun env lines ->
      Syntax.expr_string loc lines
        |> kontinue env)

  let make_expr_ident (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.symbol_of id env
        |> Syntax.expr_ident loc
        |> kontinue env

  let make_expr_un_op (start_loc, end_loc) op operand env kontinue =
    let loc = Loc.loc start_loc end_loc in
    op env (fun env op ->
      operand env (fun env operand ->
        Syntax.expr_un_op loc op operand
          |> kontinue env))

  let make_expr_bin_op (start_loc, end_loc) op lhs rhs env kontinue =
    let loc = Loc.loc start_loc end_loc in
    op env (fun env op ->
      lhs env (fun env lhs ->
        rhs env (fun env rhs ->
          Syntax.expr_bin_op loc op lhs rhs
            |> kontinue env)))

  let make_expr_let (start_loc, end_loc) binding scope env kontinue =
    let loc = Loc.loc start_loc end_loc in
    binding env (fun env binding ->
      scope env (fun env scope ->
        Syntax.expr_let loc binding scope
          |> kontinue env))

  let make_value_binding (start_loc, end_loc) patt ty value env kontinue =
    let loc = Loc.loc start_loc end_loc in
    patt env (fun env patt ->
      value env (fun env value ->
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
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.top -> 'a) -> 'a> top_test
%start top_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.binding -> 'a) -> 'a> binding_test
%start binding_test

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> term_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> expr_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> atom_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> ident_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.expr -> 'a) -> 'a> lit_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.rune -> 'a) -> 'a> rune_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.str -> 'a) -> 'a>  str_test
%start term_test
%start expr_test
%start atom_test
%start ident_test
%start lit_test
%start rune_test
%start str_test

%%

/* Test Entry Points */

top_test:
| top = top; EOF { top }

binding_test:
| binding = binding; EOF { binding }

term_test:
| term = term; EOF { term }

expr_test:
| expr = expr; EOF { expr }

atom_test:
| atom = atom; EOF { atom }

ident_test:
| ident = ident; EOF { ident }

lit_test:
| lit = lit; EOF { lit }

rune_test:
| rune = rune; EOF { rune }

str_test:
| str = str; EOF { str }

/* Top-Level Expressions */

%public top:
| "let"; binding = binding { make_top_let $sloc binding }
| "val"; binding = binding { make_top_val $sloc binding }

/* Bindings */

%public binding:
| patt = patt; ty = annot?; BIND; value = term { make_value_binding $sloc patt ty value }

/* Expressions */

%public term:
| "let"; binding = binding; "in"; scope = term { make_expr_let $sloc binding scope }
| expr = expr                                  { expr }

%public expr:
| op = un_op; operand = expr          { make_expr_un_op $sloc op operand }
| lhs = expr; op = bin_op; rhs = expr { make_expr_bin_op $sloc op lhs rhs }
| atom = atom                         { atom }

%public atom:
| lit = lit             { lit }
| id = ident            { id }
| "("; term = term; ")" { term }

%inline ident:
| id = UIDENT { make_expr_ident $sloc id }
| id = LIDENT { make_expr_ident $sloc id }

%inline lit:
| lit = BOOL                  { make_lit_bool $sloc lit }
| lit = INT                   { make_lit_int $sloc lit }
| lit = LONG                  { make_lit_long $sloc lit }
| lit = FLOAT                 { make_lit_float $sloc lit }
| lit = DOUBLE                { make_lit_double $sloc lit }
| "'"; lit = rune; "'"        { make_lit_rune $sloc lit }
| DQUOTE; lit = lines; DQUOTE { make_lit_string $sloc lit }

%public rune:
| rune = RUNE { make_rune_lit $sloc rune }
| uesc = UESC { make_rune_escape $sloc uesc }

%public lines:
| segs = list(str); NEWLINE; line = lines { segs :: line }
| segs = list(str)                        { segs :: [] }

%public str:
| str = STRING { make_str_lit $sloc str }
| uesc = UESC  { make_str_escape $sloc uesc }
