type dir = string
type file = string
type ext = string
type path = dir * file

(* Directories *)

let dir path = path

let rec mkdir dir =
  try
    if Sys.is_directory dir
    then ()
    else Unix.mkdir dir 0o755
  with Sys_error _ ->
    let parent = Filename.dirname dir in
    mkdir parent

let rec rmdir dir =
  let names = Sys.readdir dir in
  let remove_or_recurse name =
    let path = Filename.concat dir name in
    let _ =
      if Sys.is_directory path
      then rmdir path
    in
    Sys.remove name
  in
  Array.iter remove_or_recurse names

let files dir =
  Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun dir -> not (Sys.is_directory dir))

let subdirs dir =
  Sys.readdir dir
    |> Array.to_list
    |> List.filter Sys.is_directory

let in_dir dir fn x =
  let cwd = Sys.getcwd () in
  let finally _ = Unix.chdir cwd in
  let fn _ =
    Unix.chdir dir;
    fn x
  in
  Fun.protect ~finally fn

let rec temp_dir prefix =
  let id = Random.int 1000000 |> string_of_int in
  let prefix =
    if prefix = ""
    then "tmp"
    else prefix
  in
  let tmp_dir = Sys.get_temp_dir_name () in
  let dir_name = prefix ^ "-" ^ id in
  let dir = Filename.concat tmp_dir dir_name in
  Unix.mkdir dir;
  Unix.chmod dir 0o755;
  dir

let in_temp_dir =
  let dir = temp_dir () in
  in_dir dir

(* Files *)

let file name = name

let dir_sep_re = Str.quote Filename.dir_sep
let ext ext =
  let invalid msg =
    let msg = sprintf "%S is an invalid extension: %s" ext msg in
    let exn = Invalid_argument msg in
    raise exn
  in
  if ext = ""
  then invalid "blank"
  else if Str.string_match dir_sep_re ext 0
       then invalid "contains directory separator"
       else
          match String.split_on_char ext '.' with
            | [""] -> sprintf ".%s" ext
            | "" :: _ :: [] -> ext
            | _ -> invalid "contains multiple segments"

let file_ext file =
  ext (Filename.extension file)

let has_ext =
  Filename.check_suffix

let with_ext ext file =
  sprintf "%s%s" file ext

let without_ext ext file =
  match Filename.chop_suffix_opt ~suffix:ext file with
    | Some name -> name
    | None -> file

let replace_ext ext ext' file =
  match Filename.chop_suffix_opt ~suffix ext with
    | Some name -> with_ext ext' name
    | None ->
      let msg = "%S does not have suffix %S" file ext in
      let exn = Invalid_argument msg in
      raise exn

(* Paths *)

let path dir file = Filename.concat dir file

let dirname path = Filename.dirname path
let basename path = Filename.basename path

let touch path =
  let oc = open_out path in
  close_out oc

let temp_file_flags = [Open_binary; Open_wronly; Open_trunc; Open_append; Open_creat; Open_excl]
let with_temp_file write process =
  let (filename, oc) = Filename.open_temp_file ~mode:temp_file_flags "" "" in
  let close _ = close_out oc in
  let rm _ = Sys.remove filename in

  let fn _ = write oc in
  let _ = Fun.protect ~finally:close fn in

  let fn _ = process filename in
  Fun.protect ~finally:rm fn

let read_flags = [Open_binary; Open_excl; Open_rdonly]
let read read_fn path =
  let ic = open_in_gen read_flags 0 path in
  let cloase _ = close_in ic in

  let fn _ = read_fn ic in
  Fun.protect ~finally:close fn

let write_gen flags write_fn mode path =
  let process flags mode path tmpfile =
    let oc = open_out_gen write_flags mode path in
    let close _ = close_out oc in

    let fn _ = () in
    Fun.protect ~finally:close fn
  in
  with_temp_file write_fn process

let write write_fn mode path = write_gen [Open_binary; Open_trunc; Open_append; Open_excl; Open_creat; Open_wronly]
let overwrite write_fn mode path = write_gen [Open_binary; Open_trunc; Open_append; Open_excl; Open_wronly]
