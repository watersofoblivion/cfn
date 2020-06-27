open Format

(* Types *)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

type ctx = {
  docker: string;
  home:   string;
  root:   string;
}

type project = unit

type package = unit

type lockfile = unit

(* Assertions *)

let or_not_found = function
  | "" -> raise Not_found
  | str -> str

let assert_exists path =
  if Sys.file_exists path
  then path
  else
    let msg = sprintf "%s does not exist" path in
    let exn = Invalid_argument msg in
    raise exn

let assert_directory path =
  let _ = assert_exists path in
  if Sys.is_directory path
  then path
  else
    let msg = sprintf "%s is not a directory" path in
    let exn = Invalid_argument msg in
    raise exn

let assert_file path =
  let _ = assert_exists path in
  if not (Sys.is_directory path)
  then path
  else
    let msg = sprintf "%s is not a file" path in
    let exn = Invalid_argument msg in
    raise exn

let assert_executable path =
  let _ = assert_file path in
  let stat = Unix.stat path in
  if stat.st_perm land 0o100 != 0
  then path
  else
    let msg = sprintf "%s is not executable" path in
    let exn = Invalid_argument msg in
    raise exn

(* Workspace *)

let ctx =
  let docker =
    try Os.which "docker"
    with Not_found -> ""
  in
  let home =
    try Sys.getenv "CFN_HOME"
    with Not_found -> ""
  in
  let root =
    let cwd = Sys.getcwd () in
    let project_file = Os.find_in_path "project.json" cwd in
    Filename.dirname project_file
  in
  { docker = docker;
    home   = home;
    root   = root }

let docker ctx args =
  let _ = args in
  let _ =
    ctx.docker
      |> or_not_found
      |> assert_executable
  in
  ()

let cfn_home ctx =
  ctx.home
    |> or_not_found
    |> assert_directory

let dockerfile ctx =
  let _ = cfn_home ctx in
  ""

let stdlib _ _ = ()

(* Projects *)

let project _ _ = ()
let fetch prj = prj
let major _ = 0
let version _ = Semver.semver 0 0 0 [] []
let tagged_versions _ = []
let major_branches _ = []
let packages _ = []

(* Packages *)

let package _ _ = ()
let scan pkg = pkg
let imports _ = ()
let parse _ = ()
let name _ = ""
let up_to_date _ = false
let anf _ = ()

(* Lockfiles *)

let lockfile _ = ()
