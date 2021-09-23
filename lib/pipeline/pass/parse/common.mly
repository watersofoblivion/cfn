%{
  let make_optional value env kontinue = match value with
    | None -> kontinue env None
    | Some value ->
      value env (fun env value ->
        Some value
          |> kontinue env)
%}

%%
