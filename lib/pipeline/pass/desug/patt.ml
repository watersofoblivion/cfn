(* Patterns *)

open Common

let rec desug_patt env patt ty kontinue = match patt with
  | Syntax.PattGround _ -> desug_patt_ground env kontinue
  | Syntax.PattVar patt -> desug_patt_var env patt.id ty kontinue

and desug_patt_ground env kontinue =
  Annot.patt_ground
    |> kontinue env

and desug_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    Annot.patt_var id
      |> kontinue env)
