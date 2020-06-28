open Format

let rec mkdir dir =
  try
    if Sys.is_directory dir
    then ()
    else
      let msg = sprintf "%s is a file" dir in
      let exn = Invalid_argument msg in
      raise exn
  with Sys_error _ ->
    let parent = Filename.dirname dir in
    mkdir parent;
    Unix.mkdir dir 0o755

let rec rmdir dir =
  let remove_or_recurse name =
    let path = Filename.concat dir name in
    if Sys.is_directory path
    then rmdir path
    else Sys.remove path
  in
  let names = Sys.readdir dir in
  Array.iter remove_or_recurse names;
  Unix.rmdir dir

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

let rec temp_dir _ =
  let dir_name = Random.int 1000000 |> string_of_int in
  let tmp_dir = Filename.get_temp_dir_name () in
  let dir = Filename.concat tmp_dir dir_name in
  try
    Unix.mkdir dir 0o755;
    dir
  with Unix.Unix_error _ -> temp_dir ()

let in_temp_dir fn x =
  let dir = temp_dir () in
  let finally _ = rmdir dir in
  let fn _ =
    in_dir dir fn x
  in
  Fun.protect ~finally fn

let which exe =
  let rec search_path = function
    | [] -> raise Not_found
    | path :: paths ->
        let path = Filename.concat path exe in
        if Sys.file_exists path
        then path
        else search_path paths
  in
  Sys.getenv "PATH" |> String.split_on_char ':'
                    |> search_path

let rec find_in_path filename path =
  let file = Filename.concat path filename in
  if Sys.file_exists file
  then file
  else
    match path with
    | "" | "/" -> raise Not_found
    | _ ->
      let path = Filename.dirname path in
      find_in_path filename path

let read_flags = [Open_binary; Open_excl; Open_rdonly]
let read read_fn path =
  let ic = open_in_gen read_flags 0 path in
  let close _ = close_in ic in

  let fn _ = read_fn ic in
  Fun.protect ~finally:close fn

let temp_file_flags = [Open_binary; Open_wronly; Open_trunc; Open_append; Open_creat; Open_excl]
let write_gen flags write_fn mode path =
  let (tempfile, oc) = Filename.open_temp_file ~mode:temp_file_flags "" "" in
  let finally _ = close_out oc in
  let fn _ = write_fn oc in
  Fun.protect ~finally fn;

  let dir = Filename.dirname path in
  mkdir dir;

  let ic = open_in_gen read_flags 0 tempfile in
  let oc = open_out_gen flags mode path in
  let finally _ =
    close_in ic;
    close_out oc;
    Sys.remove tempfile
  in
  let fn _ =
    let buf_len = in_channel_length ic in
    let buf = Bytes.create buf_len in
    let rec copy ic =
      match input ic buf 0 buf_len with
        | 0 -> ()
        | read ->
          output oc buf 0 read;
          copy ic
    in
    copy ic;
    Unix.chmod path mode
  in
  Fun.protect ~finally fn

let write = write_gen [Open_binary; Open_trunc; Open_append; Open_excl; Open_creat; Open_wronly]
let overwrite = write_gen [Open_binary; Open_trunc; Open_append; Open_creat; Open_wronly]
