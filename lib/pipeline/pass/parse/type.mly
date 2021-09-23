%{
  [@@@coverage exclude_file]
  open Common

  let make_ty_constr (start_pos, end_pos) constr env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Env.constr_of constr env (fun env sym ->
      Syntax.ty_constr loc sym
        |> kontinue env)
%}

/* Testing Entry Points */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.ty -> 'a) -> 'a> annot_test
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.ty -> 'a) -> 'a> ty_test
%start annot_test
%start ty_test

%%

/* Test Entry Point */

annot_test:
| annot = annot; EOF { annot }

ty_test:
| ty = ty; EOF { ty }

/* Types */

%public annot:
| ":"; ty = ty { ty }

%public ty:
| constr = UIDENT { make_ty_constr $sloc constr }
