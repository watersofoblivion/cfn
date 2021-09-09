%{
  [@@@coverage exclude_file]

  let rec make_tops tops env kontinue = match tops with
    | [] -> kontinue env []
    | top :: tops ->
      top env (fun env top ->
        make_tops tops env (fun env tops ->
          top :: tops
            |> kontinue env))

  let rec make_imports imports env kontinue = match imports with
    | [] -> kontinue env []
    | import :: imports ->
      import env (fun env import ->
        make_imports imports env (fun env imports ->
          import :: imports
            |> kontinue env))

  let make_file pkg imports tops env kontinue =
    pkg env (fun env pkg ->
      make_imports imports env (fun env imports ->
        make_tops tops env (fun env tops ->
          Syntax.file pkg imports tops
            |> kontinue env)))
%}

/* Source Files */

%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.file -> 'a) -> 'a> package_only
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.file -> 'a) -> 'a> imports_only
%type <Syntax.ty Common.Env.t -> (Syntax.ty Common.Env.t -> Syntax.file -> 'a) -> 'a> file
%start package_only
%start imports_only
%start file

%%

/* Source Files */

%public package_only:
| pkg = pkg; end_of_package_only { make_file pkg [] [] }

end_of_package_only:
| "from"              { () }
| "import"            { () }
| end_of_imports_only { () }

%public imports_only:
| pkg = pkg; imports = list(import); end_of_imports_only { make_file pkg imports [] }

end_of_imports_only:
| "let"       { () }
| "val"       { () }
| end_of_file { () }

%public file:
| pkg = pkg; imports = list(import); tops = list(top); end_of_file { make_file pkg imports tops }

end_of_file:
| EOF { () }
