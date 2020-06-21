open Format

type t = {
  clang:  string;
  docker: string;
  home:   string;
  root:   string
}

(* Constructor *)

let empty =
  { clang  = "";
    docker = "";
    home   = "";
    root   = "" }

let or_not_found = function
  | "" -> raise Not_found
  | str -> str

let clang_exe ctx = or_not_found ctx.clang
let docker_exe ctx = or_not_found ctx.docker
let cfn_home ctx = or_not_found ctx.home
let project_root ctx = or_not_found ctx.root

(* Assertions *)

let assert_exists path =
  if Sys.file_exists path
  then ()
  else
    let msg = sprintf "%s does not exist" path in
    let exn = Invalid_argument msg in
    raise exn

let assert_directory path =
  assert_exists path;
  if Sys.is_directory path
  then ()
  else
    let msg = sprintf "%s is not a directory" path in
    let exn = Invalid_argument msg in
    raise exn

let assert_file path =
  assert_exists path;
  if not (Sys.is_directory path)
  then ()
  else
    let msg = sprintf "%s is not a file" path in
    let exn = Invalid_argument msg in
    raise exn

let assert_executable path =
  assert_file path;
  let stat = Unix.stat path in
  if stat.st_perm land 0o100 != 0
  then ()
  else
    let msg = sprintf "%s is not executable" path in
    let exn = Invalid_argument msg in
    raise exn

(* Builders *)

let with_clang path ctx =
  assert_executable path;
  { ctx with clang = path }

let with_docker path ctx =
  assert_executable path;
  { ctx with docker = path }

let with_cfn_home path ctx =
  assert_directory path;
  { ctx with home = path }

let with_project_root path ctx =
  assert_directory path;
  { ctx with root = path }

let from_env ctx =
  let cwd = Sys.getcwd () in
  let project_file = Os.find_in_path "project.json" cwd in
  let root = Filename.dirname project_file in

  ctx |> with_cfn_home (Sys.getenv "CFN_HOME")
      |> with_docker (Os.which "docker")
      |> with_clang (Os.which "clang")
      |> with_project_root root

(* Path Helpers *)

let cfn_stdlib_dir = "stdlib"
let cfn_stdlib ctx =
  let home = cfn_home ctx in
  Filename.concat home cfn_stdlib_dir

let cfn_shared_dir = "shared"
let cfn_shared ctx =
  let home = cfn_home ctx in
  Filename.concat home cfn_shared_dir

let build_dir = ".cfn++"
let build_path ctx = Filename.concat ctx.root build_dir

let src_dir = "src"
let src_path ctx =
  let build_path = build_path ctx in
  Filename.concat build_path src_dir

let lib_dir = "lib"
let lib_path ctx =
  let build_path = build_path ctx in
  Filename.concat build_path lib_dir
