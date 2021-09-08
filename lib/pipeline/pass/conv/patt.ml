(* Patterns *)

open Common

let rec convert_patt env patt ty kontinue = match patt with
  | Mono.PattGround -> convert_patt_ground env kontinue
  | Mono.PattVar patt -> convert_patt_var env patt.id ty kontinue

and convert_patt_ground env kontinue =
  Clos.patt_ground
    |> kontinue env

and convert_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    id
      |> Clos.patt_var
      |> kontinue env)
