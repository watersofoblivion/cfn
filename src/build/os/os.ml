open Format

(* Operating System *)

(* Assertion Helpers *)

let fail_path_assert path msg =
  failwith (sprintf "%S %s" path msg)
let fail_path_exists path =
  fail_path_assert path "does not exist"

(* Path Assertions *)

let assert_file path =
  try
    if Sys.is_directory path
    then fail_path_assert path "is a directory"
  with Sys_error _ -> fail_path_exists path

let assert_executable path =
  assert_file path;
  let stat = Unix.stat path in
  if stat.st_perm land 0o111 <> 0o111
  then fail_path_assert path "is not executable"

let assert_directory path =
  try
    if not (Sys.is_directory path)
    then fail_path_assert path "is not a directory"
  with Sys_error _ -> fail_path_exists path

(* Finding Files *)

let which exe =
  let rec search_path = function
    | [] -> failwith (sprintf "%S not found in ${PATH}" exe)
    | path :: paths ->
        let path = Filename.concat path exe in
        try
          assert_executable path;
          path
        with _ -> search_path paths
  in
  getenv "PATH" |> String.split_on_char ':'
                |> search_path

let rec find_in_path filename base path =
  try
    let path = filename
            |> Filename.concat path
            |> Filename.concat base
    in
    assert_file path;
    Filename.dirname path
  with _ ->
    match path with
      | "" | "/" -> failwith (sprintf "%S not found within %S" filename base)
      | _ -> find_in_path filename base (Filename.dirname path)
