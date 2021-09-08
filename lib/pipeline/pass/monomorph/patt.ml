(* Patterns *)

open Common

let rec convert_patt env patt ty kontinue = match patt with
  | Ir.PattGround -> convert_patt_ground env kontinue
  | Ir.PattVar patt -> convert_patt_var env patt.id ty kontinue

and convert_patt_ground env kontinue =
  Mono.patt_ground
    |> kontinue env

and convert_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    id
      |> Mono.patt_var
      |> kontinue env)
