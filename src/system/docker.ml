open Format

(* Command *)

let docker args handler = Os.run "docker" args handler

(* Images *)

type i = {
  img_name: string;
  img_tag:  string
}

let image name tag =
  { img_name = name;
    img_tag  = tag }
let img_name img = img.img_name
let img_tag img = img.img_tag
let img_id img = sprintf "%s:%s" img.img_name img.img_tag

let build dir name tag =
  let id = sprintf "%s:%s" name tag in
  let _ = docker ["build"; "-t"; id; dir] Os.ignore in
  image name tag

(* Containers *)

type c = unit

(* Operations *)

let run _ = ()
