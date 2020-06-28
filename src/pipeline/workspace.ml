open Format

(* Constants *)

let project_file_name = "cfn++.prj"
let lock_file_name = "cfn++.lock"

let cache_dir = ".cfn++"
let src_dir = "src"
let lib_dir = "lib"
let pkg_dir = "pkg"

(* Types *)

module ProjectMap = Map.Make(struct
  type t = Path.project
  let compare = Path.compare_project
end)

module PackageMap = Map.Make(struct
  type t = Path.package
  let compare = Path.compare_package
end)

type package = {
  name:        string;
  path:        Path.package;
  dir:         string;
  cfn_files:   string list;
  other_files: string list;
  anf_file:    string;
  mtime:       float;
}

type project = {
  path:         Path.project;
  dir:          string;
  packages:     package PackageMap.t;
  dependencies: project ProjectMap.t;
  version:      Semver.t;
  commit:       string
}

type t = {
  docker:       string;
  home:         string;
  root:         string;
  project:      Path.project;
  current:      package PackageMap.t;
  dependencies: project ProjectMap.t
}

(* Assertions *)
(*
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
*)
let assert_not_exists path =
  if Sys.file_exists path
  then
    let msg = sprintf "%s exists" path in
    let exn = Invalid_argument msg in
    raise exn
  else path

let assert_absolute path =
  if Filename.is_relative path
  then
    let msg = sprintf "%s is a relative path" path in
    let exn = Invalid_argument msg in
    raise exn
  else path
(*
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
    raise exn *)

(* Constructors *)

let from_env _ =
  let docker =
    try Os.which "docker"
    with Not_found -> ""
  in
  let home =
    try Sys.getenv "CFN_HOME"
    with Not_found -> ""
  in
  { docker       = docker;
    home         = home;
    root         = "";
    project      = Path.project ".";
    current      = PackageMap.empty;
    dependencies = ProjectMap.empty }

(* let read_project_file ws = *)


let current _ =
  let root =
    let cwd = Sys.getcwd () in
    let project_file = Os.find_in_path "project.json" cwd in
    Filename.dirname project_file
  in
  let ws =
    let ws = from_env () in
    { ws with root = root }
  in
  ws

let write_project_file ws =
  let project_file =
    { Files_t.name = Path.source ws.project }
  in
  let write oc =
    project_file
      |> Files_j.string_of_project_file
      |> Yojson.Safe.prettify
      |> output_string oc
  in
  project_file_name
    |> Filename.concat ws.root
    |> Os.write write 0o644

let write_lockfile ws =
  let lockfile =
    let fn (_, prj) =
      let vcs = match Path.vcs prj.path with
        | Path.Git -> Files_t.Git
      in
      { Files_t.source  = Path.source prj.path;
        Files_t.major   = Path.major prj.path;
        Files_t.vcs     = vcs;
        Files_t.version = prj.version;
        Files_t.commit  = prj.commit }
    in
    { Files_t.dependencies = ws.dependencies
                               |> ProjectMap.bindings
                               |> List.map fn }
  in
  let write oc =
    lockfile
      |> Files_j.string_of_lock_file
      |> Yojson.Safe.prettify
      |> output_string oc
  in
  lock_file_name
    |> Filename.concat ws.root
    |> Os.overwrite write 0o644

let create prj dir =
  let _ =
    dir
      |> assert_absolute
      |> assert_not_exists
  in
  let ws =
    let ws = from_env () in
    { ws with root    = dir;
              project = prj }
  in
  let _ = Os.mkdir dir in
  let _ = write_project_file ws in
  let _ = write_lockfile ws in
  ws

(* Workspaces *)

(* let fetch _ ws = ws *)

(* let docker ctx args =
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

let stdlib _ _ = () *)

(* Projects *)

(* let major _ = 0
let version _ = Semver.semver 0 0 0 [] []
let tagged_versions _ = []
let major_branches _ = []
let packages _ = [] *)

(* Packages *)

(* let scan pkg = pkg
let imports _ = ()
let parse _ = ()
let name _ = ""
let up_to_date _ = false
let anf _ = () *)
