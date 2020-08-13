open Format

(* Directory Handling *)

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

let dirs dir =
  Sys.readdir dir
    |> Array.to_list
    |> List.filter Sys.is_directory

let subdirs root =
  let rec find_dirs dirs path =
    let abs = Filename.concat root path in
    let filter_map subdir =
      if subdir
           |> Filename.concat abs
           |> Sys.is_directory
      then
        let path = Filename.concat path subdir in
        Some path
      else None
    in
    abs
      |> Sys.readdir
      |> Array.to_list
      |> List.filter_map filter_map
      |> List.fold_left find_dirs dirs
      |> List.cons path
  in
  find_dirs [] ""

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

(* Filesystem Searching *)

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

(* Atomic File Operations *)

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

(* Process Control *)

let joiner = Bytes.create 0

type output =
  | Out of bytes
  | Err of bytes

let of_stdout bs = Out bs
let of_stderr bs = Err bs

exception NonZero of int * output list

let output_stream filter output =
  output
    |> List.filter_map filter
    |> Bytes.concat joiner

let filter_stdout = function
  | Out buf -> Some buf
  | _ -> None

let filter_stderr = function
  | Err buf -> Some buf
  | _ -> None

let stdout = output_stream filter_stdout
let stderr = output_stream filter_stderr

let combined output =
  let map = function
    | Out buf -> buf
    | Err buf -> buf
  in
  output
    |> List.map map
    |> Bytes.concat joiner

let dump stdout stderr process_output =
  let iter = function
    | Out buf -> output stdout buf 0 (Bytes.length buf)
    | Err buf -> output stderr buf 0 (Bytes.length buf)
  in
  List.iter iter process_output

let lines bs =
  let rec trim_head = function
    | "" :: lines -> trim_head lines
    | lines -> lines
  in
  let rec trim_tail = function
    | [] -> []
    | "" :: lines ->
      begin
        match trim_tail lines with
          | [] -> []
          | lines -> "" :: lines
      end
    | hd :: lines -> hd :: (trim_tail lines)
  in
  bs
    |> Bytes.to_string
    |> String.split_on_char '\n'
    |> trim_head
    |> trim_tail

let first_line bs = match lines bs with
  | [] ->
    let exn = Invalid_argument "no output lines" in
    raise exn
  | line :: _ -> line

let line bs = match lines bs with
  | line :: [] -> line
  | lines ->
    let msg = sprintf "expected 1 line, found %d" (List.length lines) in
    let exn = Invalid_argument msg in
    raise exn

let ignore _ = ()

let run cmd args handler =
  let exe = which cmd in
  let args =
    args
      |> List.cons cmd
      |> Array.of_list
  in

  let proc = Unix.open_process_args_full exe args [||] in

  let buflen = 1024 in
  let (out, _, err) = proc in
  let out_fd = Unix.descr_of_in_channel out in
  let err_fd = Unix.descr_of_in_channel err in
  let finally _ =
    close_in out;
    close_in err
  in
  let read_output _ =
    let rec read_output out = function
      | [] -> out
      | source_fds ->
        let (out, source_fds) = match Unix.select source_fds [] [] (-1.0) with
          | ([], _, _) -> (out, source_fds)
          | (ready_fds, _, _) ->
            let fold (out, source_fds) ready_fd =
              let bs = Bytes.create buflen in
              match Unix.read ready_fd bs 0 buflen with
                | 0 ->
                  let filter source_fd = source_fd != ready_fd in
                  (out, List.filter filter source_fds)
                | n ->
                  let bs = Bytes.sub bs 0 n in
                  let segment =
                    if ready_fd = out_fd
                    then of_stdout bs
                    else of_stderr bs
                  in
                  (segment :: out, source_fds)
            in
            List.fold_left fold (out, source_fds) ready_fds
        in
        read_output out source_fds
    in
    [out_fd; err_fd]
      |> read_output []
      |> List.rev
  in
  let output = Fun.protect ~finally read_output in

  let pid = Unix.process_full_pid proc in
  match Unix.waitpid [] pid with
    | (_, Unix.WEXITED 0) ->
      let filter_map = function
        | Out bs -> Some bs
        | _ -> None
      in
      output
        |> List.filter_map filter_map
        |> Bytes.concat joiner
        |> handler
    | (_, Unix.WEXITED exit_status) ->
      let exn = NonZero(exit_status, output) in
      raise exn
    | (_, Unix.WSIGNALED signum) ->
      let msg = sprintf "child %d received unexpected signal %d" pid signum in
      failwith msg
    | (_, Unix.WSTOPPED signum) ->
      let msg = sprintf "child %d stopped by unexpected signal %d" pid signum in
      failwith msg
