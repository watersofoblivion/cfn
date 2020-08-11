(* open Format

(* Exceptions *)

let invalid_argument msg =
  let exn = Invalid_argument msg in
  raise exn

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
    path
      |> sprintf "%s exists"
      |> invalid_argument
  else path

let assert_absolute path =
  if Filename.is_relative path
  then
    path
      |> sprintf "%s is not an absolute path"
      |> invalid_argument
  else path
(*
let assert_directory path =
  let _ = assert_exists path in
  if path
       |> assert_exists
       |> Sys.is_directory
  then path
  else
    path
      |> sprintf "%s is not a directory"
      |> invalid_argument

let assert_file path =
  let _ = assert_exists path in
  if path
       |> assert_exists
       |> Sys.is_directory
       |> not
  then path
  else
    path
      |> sprintf "%s is not a file"
      |> invalid_argument

let assert_executable path =
  let stat =
    path
      |> assert_file
      |> Unix.stat
  in
  if stat.st_perm land 0o111 != 0
  then path
  else
    let msg = sprintf "%s is not executable" path in
    let exn = Invalid_argument msg in
    raise exn *)

(* Types *)

type package = {
  name:        string;
  dir:         string;
  cfn_files:   string list;
  other_files: string list;
  src_mtime:   float;
  anf_file:    string;
  anf_mtime:   float;
}

type project = {
  id:           string;
  major:        string;
  dependencies: project list;
  packages:     package list;
  version:      Semver.t;
  commit:       string
}

type t = {
  docker:  string;
  home:    string;
  root:    string;
  id:      Path.id;
  current: project
}

(* Paths *)

let project_file ws = Filename.concat ws.root "cfn++.proj"
let lock_file ws = Filename.concat ws.root "cfn++.lock"

let cache_dir ws = Filename.concat ws.root ".cfn++"
let src_dir ws =
  "src"
    |> Filename.concat (cache_dir ws)

let lib_dir ws =
  "lib"
    |> Filename.concat (cache_dir ws)

let proj_dir ws proj =
  proj
    |> Path.source
    |> Filename.concat (src_dir ws)

let pkg_dir ws proj pkg =
  let proj_dir = proj_dir ws proj in
  pkg.pkg_path
    |> Path.path
    |> Filename.concat proj_dir

let anf_file ws proj pkg =
  "package.anf"
    |> Filename.concat (Path.path pkg.pkg_path)
    |> Filename.concat (lib_dir ws)

(* Workspaces *)

let from_env _ =
  let docker =
    try Os.which "docker"
    with Not_found -> ""
  in
  let home =
    try Sys.getenv "CFN_HOME"
    with Not_found -> ""
  in
  let current =
    { proj_path    = Path.project ".";
      (* dependencies = ProjectMap.empty; *)
      packages     = PackageMap.empty;
      version      = Semver.semver 0 0 0 [] [];
      commit       = "" }
  in
  { docker  = docker;
    home    = home;
    root    = "";
    id      = Path.id "from-env";
    current = current }

let with_root root ws = { ws with root = root }
let with_id id ws = { ws with id = id }

(* Projects *)

let with_packages proj pkgs = { proj with packages = pkgs }
(* let with_dependencies proj deps = { proj with dependencies = deps } *)

(* Lockfiles *)

let read_json deserialize ic =
  let lexer_state = Yojson.init_lexer () in
  let lexbuf = Lexing.from_channel ic in
  deserialize lexer_state lexbuf

let write_json serialize value oc =
  value
    |> serialize
    |> Yojson.Safe.prettify
    |> output_string oc

let read_project_file = read_json Files_j.read_project_file
let write_project_file = write_json Files_j.string_of_project_file

let read_lock_file = read_json Files_j.read_lock_file
let write_lock_file = write_json Files_j.string_of_lock_file

(* Lock *)

let lock ws =
  let lockfile =
    let fn (_, proj) =
      let vcs = match Path.vcs proj.proj_path with
        | Path.Git -> Files_t.Git
      in
      { Files_t.source  = Path.source proj.proj_path;
        Files_t.major   = Path.major proj.proj_path;
        Files_t.vcs     = vcs;
        Files_t.version = proj.version;
        Files_t.commit  = proj.commit }
    in
    { Files_t.dependencies = ws.dependencies
                               |> ProjectMap.bindings
                               |> List.map fn }
  in
  let write = write_lock_file lockfile in
  ws
    |> lock_file
    |> Os.overwrite write 0o644

(* Current *)

let load_project_file ws =
  let project_file =
    ws
      |> project_file
      |> Os.read read_project_file
  in
  let id = Path.id project_file.id in
  with_id id ws

let load_lock_file ws =
  let load ws dep =
    let path =
      let vcs = match dep.Files_j.vcs with
        | Files_j.Git -> Path.Git
      in
      let ext = Path.vcs_ext vcs in
      let path = sprintf "%s%s@v%d" dep.source ext dep.major in
      Path.project path
    in
    let src_dir =
      path
        |> Path.source
        |> Filename.concat src_dir
        |> Filename.concat cache_dir
        |> Filename.concat ws.root
    in
    let project =
      { proj_path = path;
        src_dir   = src_dir;
        packages  = PackageMap.empty;
        available = Semver.empty;
        version   = dep.version;
        commit    = dep.commit }
    in
    { ws with dependencies = ProjectMap.add path project ws.dependencies }
  in

  let path = Filename.concat ws.root lock_file_name in
  if Sys.file_exists path
  then
    let lock_file = Os.read read_lock_file path in
    List.fold_left load ws lock_file.dependencies
  else
    ws

let current _ =
  let root =
    ()
      |> Sys.getcwd
      |> Os.find_in_path project_file_name
      |> Filename.dirname
  in
  ()
    |> from_env
    |> with_root root
    |> load_project_file
    |> load_lock_file

(* Create *)

let create id dir =
  dir
    |> assert_absolute
    |> assert_not_exists
    |> Os.mkdir;

  let ws =
    ()
      |> from_env
      |> with_root dir
      |> with_id id
  in

  let write = write_project_file { Files_t.id = Path.name id } in
  project_file_name
    |> Filename.concat ws.root
    |> Os.write write 0o644;

  lock ws;
  ws

(* Scan *)

let scan_package ws proj dir =
  let pkg =
    let pkg =
      { name        = "";
        pkg_path    = Path.package pkg_dir;
        dir         = "";
        cfn_files   = [];
        other_files = [];
        src_mtime   = 0.0;
        anf_file    = "";
        anf_mtime   = 0.0 }
    in
    { pkg with dir      = pkg_dir ws proj pkg;
               anf_file = anf_file ws proj pkg }
  in

  let anf_mtime =
    if Sys.file_exists pkg.anf_file
    then
      let stat = Unix.stat pkg.anf_file in
      stat.st_mtime
    else
      0.0
  in

  let fold pkg filename =
    let mtime =
      let path =
        filename
          |> Filename.concat pkg.dir
      in
      let stat = Unix.stat path in
      max stat.st_mtime pkg.src_mtime
    in
    match Filename.extension filename with
      | ".cfn" ->
        { pkg with src_mtime = mtime;
                   cfn_files = filename :: pkg.cfn_files }
      | _ ->
        { pkg with src_mtime   = mtime;
                   other_files = filename :: pkg.other_files }
  in
  src_dir
    |> Os.files
    |> List.fold_left fold pkg

let scan_project ws proj =
  let map = scan_package ws proj in
  let fold pkgs pkg = PackageMap.add pkg.pkg_path pkg pkgs in
  let path =
    let path = Path.source proj.proj_path in
    proj.proj_path
      |> Path.major
      |> sprintf "%s@v%d" path
  in
  path
    |> Filename.concat src_dir
    |> Filename.concat cache_dir
    |> Filename.concat ws.root
    |> Os.subdirs
    |> List.map map
    |> List.fold_left fold PackageMap.empty
    |> with_packages proj

let scan ws =
  let scan = scan_project ws in
  ws.dependencies
    |> ProjectMap.map scan
    |> with_dependencies ws

(* Package Dependencies *)

let package_deps pkg =
  let fold projs filename =
    let file =
      filename
        |> Filename.concat pkg.dir
        |> Syntax.Lexer.from_file
        |> Syntax.Parser.imports_only Syntax.Lexer.lex
    in
    let filter_map import_stmt = match import_stmt.Syntax.Ast.from with
      | Some from ->
        let path = Path.project from.import_path in
        Some path
      | None -> None
    in
    let fold projs proj = ProjectSet.add proj projs in
    file.import_stmts
      |> List.filter_map filter_map
      |> List.fold_left fold projs
  in
  pkg.cfn_files
    |> List.fold_left fold ProjectSet.empty

(* Fetch *)

let fetch_project proj ws =
  let _ = proj in
  ws

let fetch ws projs =
  ws
    |> List.fold_right fetch_project projs

let fetch_all ws =
  let current =
    let proj =
      { proj_path  = Path.project ".";
        src_dir   = ws.root;
        packages  = PackageMap.empty;
        available = Semver.empty;
        version   = Semver.semver 0 0 0 [][];
        commit    = "" }
    in
    scan_project ws proj
  in

  let fold_pkg_deps _ pkg acc =
    pkg
      |> package_deps
      |> ProjectSet.union acc
  in
  let fold_ws_deps k _ = ProjectSet.add k in
  ProjectSet.empty
    |> PackageMap.fold fold_pkg_deps current.packages
    |> ProjectMap.fold fold_ws_deps ws.dependencies
    |> ProjectSet.elements
    |> fetch ws

(* Update *)

let update_project ?pre_release:(pre_release = false) proj ws =
  let _ = pre_release in
  let _ = proj in
  ws

let update ?pre_release:(pre_release = false) projs ws =
  ws
    |> List.fold_right (update_project ~pre_release) projs

let update_all ?pre_release:(pre_release = false) ws =
  let update =
    ws.dependencies
      |> ProjectMap.bindings
      |> List.map fst
      |> update ~pre_release
  in
  update ws *)
