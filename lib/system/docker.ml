open Format

(* Command *)

let docker args handler = Os.run "docker" args handler

(* Images *)

type i = {
  img_name: string;
  img_tag:  string;
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
  { img_name = name;
    img_tag  = tag  }

(* Operations *)

type mount = {
  local:     string;
  container: string;
}

let local_dir m = m.local
let container_dir m = m.container

let dir_mount local_dir container_dir =
  let invalid_argument msg =
    let exn = Invalid_argument msg in
    raise exn
  in
  let assert_absolute msg dir =
    if Filename.is_relative dir
    then
      sprintf "%s dir %S is not absolute" msg dir
        |> invalid_argument
    else dir
  in
  let assert_exists dir =
    try
      if Sys.is_directory dir
      then dir
      else
        sprintf "%S is not a directory" dir
          |> invalid_argument
    with Sys_error _ ->
      sprintf "%S does not exist" dir
        |> invalid_argument
  in
  { local     = local_dir     |> assert_absolute "local" |> assert_exists;
    container = container_dir |> assert_absolute "container" }

type mounts = mount list
let mounts = []
let add_mount = List.cons

let run_in cmd args ?mounts:(mounts = mounts) img handler =
  let img_id = img_id img in
  let args = img_id :: cmd :: args in
  let fold_mounts args mount =
    let local_dir = local_dir mount in
    let container_dir = container_dir mount in
    let arg = sprintf "type=bind,source=%s,target=%s" local_dir container_dir in
    "--mount" :: arg :: args
  in
  let args = List.fold_left fold_mounts args mounts in
  let args = "run" :: args in
  docker args handler
