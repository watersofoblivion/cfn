(* Patterns *)

open Common

let rec norm_patt env patt ty kontinue = match patt with
  | Annot.PattGround -> norm_patt_ground env kontinue
  | Annot.PattVar patt -> norm_patt_var env patt.id ty kontinue

and norm_patt_ground env kontinue =
  Ir.patt_ground
    |> kontinue env

and norm_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    Ir.patt_var id
      |> kontinue env)
