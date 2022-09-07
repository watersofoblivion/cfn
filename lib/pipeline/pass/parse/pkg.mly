%{
  [@@@coverage exclude_file]
  open Common

  let make_pkg (start_pos, end_pos) id env kontinue =
    let loc = Loc.loc start_pos end_pos in
    id env (fun env id ->
      Syntax.pkg loc id
        |> kontinue env)
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.pkg -> 'a) -> 'a> pkg_test
%start pkg_test

%%

/* Test Entry Points */

pkg_test:
| pkg = pkg; EOF { pkg }

/* Package Statement */

%public pkg:
| "package"; id = name { make_pkg $sloc id }
