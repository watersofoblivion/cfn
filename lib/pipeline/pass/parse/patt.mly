%{
  [@@@coverage exclude_file]
  open Common

  let make_patt_ground (start_loc, end_loc) env kontinue =
    Loc.loc start_loc end_loc
      |> Syntax.patt_ground
      |> kontinue env

  let make_patt_var (start_loc, end_loc) id env kontinue =
    let loc = Loc.loc start_loc end_loc in
    Env.rename id env (fun env sym ->
      Syntax.patt_var loc sym
        |> kontinue env)
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.patt -> 'a) -> 'a> patt_test
%start patt_test

%%

/* Test Entry Points */

patt_test:
| patt = patt; EOF { patt }

/* Patterns */

%public patt:
| "_"         { make_patt_ground $sloc }
| id = LIDENT { make_patt_var $sloc id }
