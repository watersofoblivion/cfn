(* Command *)

let docker args handler = Os.run "docker" args handler

(* Images *)

type i = unit

(* Containers *)

type c = unit

(* Operations *)

let run _ = ()
