(* Patterns *)

open Common

let rec mono_patt env patt ty kontinue = match patt with
  | Ir.PattGround -> mono_patt_ground env kontinue
  | Ir.PattVar patt -> mono_patt_var env patt.id ty kontinue

and mono_patt_ground env kontinue =
  Mono.patt_ground
    |> kontinue env

and mono_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    id
      |> Mono.patt_var
      |> kontinue env)
