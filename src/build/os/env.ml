type var = string

let var name = name

let get var = Sys.getenv var
let get_opt var =
  try Some (get var)
  with Not_found -> None

let set var value = Unix.putenv var value
