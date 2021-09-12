%{
  [@@@coverage exclude_file]
  open Common

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

%%

/* Test Entry Points */

top_test:
| top = top; EOF { top }

/* Top-Level Expressions */

%public top:
| "let"; binding = binding { make_top_let $sloc binding }
| "val"; binding = binding { make_top_val $sloc binding }
