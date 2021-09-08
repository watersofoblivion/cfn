(* Source Files *)

let rec desug_tops env tops kontinue = match tops with
  | [] -> kontinue env []
  | top :: tops ->
    Ast.desug_top env top (fun env top ->
      desug_tops env tops (fun env tops ->
        top :: tops
          |> kontinue env))

(* TODO: More than just tops in order *)
let desug_file env file kontinue = match file with
  | Syntax.File file -> desug_tops env file.tops kontinue
