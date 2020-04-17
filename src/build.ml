open Format

(* Utility Functions *)

(* Environment *)

let getenv var =
  try Sys.getenv var
  with Not_found ->
    failwith (sprintf "${%s} not set" var)

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

(* Filesystem Helpers *)

let mkpath segments =
  let cwd = Sys.getcwd () in

  let is_relative_to rel path =
    let prefix = String.sub path 0 (String.length rel) in
    prefix = rel
  in
  let is_relative_to_current_dir = is_relative_to Filename.current_dir_name in
  let is_relative_to_parent_dir = is_relative_to Filename.parent_dir_name in

  let strip_prefix prefix path =
    let prefix_len = String.length prefix in
    let path_len = String.length path in
    String.sub path (prefix_len + 1) (path_len - prefix_len - 1)
  in

  let init, segments =
    match segments with
    | [] -> (cwd, [])
    | hd :: tl ->
        (* Absolute path *)
        if not (Filename.is_relative hd) then
          (hd, tl)

        (* ".." or "../" *)
        else if hd = Filename.parent_dir_name
                || hd = Filename.concat Filename.parent_dir_name "" then
          (Filename.dirname cwd, [])

        (* "." or "./" *)
        else if hd = Filename.current_dir_name
                || hd = Filename.concat Filename.current_dir_name "" then
          (cwd, [])

        (* "../rest/of/path" *)
        else if is_relative_to_parent_dir hd then
          let hd = strip_prefix Filename.parent_dir_name hd in
          (Filename.dirname cwd, hd :: tl)

        (* "./rest/of/path" *)
        else if is_relative_to_current_dir hd then
          let hd = strip_prefix Filename.current_dir_name hd in
          (cwd, hd :: tl)

        (* Relative not starting with "." or ".." *)
        else if Filename.is_implicit hd then
          (cwd, segments)

        (* Something is *really* jacked up! *)
        else failwith (sprintf "Invalid first segment of path: %S" hd)
  in
  List.fold_left Filename.concat init segments

let mkdir_p path =
  let rec mkdir_p = function
    | "" as path -> path
    | "/" as path -> path
    | path ->
      try
        if Sys.is_directory path
        then path
        else failwith (sprintf "%S exists and is not a directory" path)
      with Sys_error _ ->
        let dirname = Filename.dirname path in
        let _ = mkdir_p dirname in
        Unix.mkdir path 0o755;
        path
  in
  mkdir_p (mkpath [ path ])


let in_dir dir f x =
  let dir = mkpath [ dir ] in
  let old_dir = Sys.getcwd () in
  Unix.chdir dir ;
  try
    let res = f x in
    Unix.chdir old_dir;
    res
  with exn ->
    Unix.chdir old_dir;
    raise exn

let rec rm_rf dir =
  let dir = mkpath [dir] in
  let entries = Sys.readdir dir in
  let rm_or_recurse f =
    let filename = Filename.concat dir f in
    if Sys.is_directory filename
    then rm_rf filename
    else Sys.remove filename
  in
  Array.iter rm_or_recurse entries;
  Unix.rmdir dir


let in_temp_dir f x =
  let tmpdir = Filename.get_temp_dir_name () in
  let rand = Random.int 1000000 |> string_of_int in
  let tmpdir = Filename.concat tmpdir rand in
  Unix.mkdir tmpdir 0o755 ;
  try
    let res = in_dir tmpdir f x in
    rm_rf tmpdir;
    res
  with exn ->
    rm_rf tmpdir;
    raise exn

(* Build Contexts *)

type ctx = {
  clang:  string;
  stdlib: string;
  home:   string;
  root:   string
}

let empty_ctx =
  { clang  = "";
    stdlib = "";
    home   = "";
    root   = "" }

let ctx_with_clang path ctx =
  assert_executable path;
  { ctx with clang = path }

let ctx_with_stdlib path ctx =
  assert_directory path;
  { ctx with stdlib = path }

let ctx_with_home path ctx =
  assert_directory path;
  { ctx with home = path }

let ctx_with_root path ctx =
  assert_directory path;
  { ctx with root = path }

let ctx_from_env ctx =
  ctx |> ctx_with_stdlib (getenv "CFNROOT")
      |> ctx_with_home (getenv "HOME")
      |> ctx_with_clang (which "clang")
      |> ctx_with_root (find_in_path "project.json" "" (Sys.getcwd ()))
