type t = {
  clang:  string;
  stdlib: string;
  home:   string;
  root:   string
}

(* Constructor *)

let ctx =
  { clang  = "";
    stdlib = "";
    home   = "";
    root   = "" }

(* Builders *)

let with_clang path ctx =
  assert_executable path;
  { ctx with clang = path }

let with_stdlib path ctx =
  assert_directory path;
  { ctx with stdlib = path }

let with_home path ctx =
  assert_directory path;
  { ctx with home = path }

let with_root path ctx =
  assert_directory path;
  { ctx with root = path }

let from_env ctx =
  ctx |> with_stdlib (getenv "CFNROOT")
      |> with_home (getenv "HOME")
      |> with_clang (which "clang")
      |> with_root (find_in_path "project.json" "" (Sys.getcwd ()))

(* Path Helpers *)

let build_dir ctx = ""
let src_dir ctx import_path = ""
let lib_dir ctx import_path = ""
let pkg_dir ctx = ""
