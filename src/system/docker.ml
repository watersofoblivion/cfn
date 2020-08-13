open Format

(* Command *)

let docker args handler =
  try Os.run "docker" args handler
  with Os.NonZero(exit_status, output) ->
    let _ = Os.dump Stdlib.stdout Stdlib.stderr output in
    let msg = sprintf "exited with status %d" exit_status in
    failwith msg

(* Images *)

type i = unit

(* Containers *)

type c = unit

(* Operations *)

let run _ = ()
