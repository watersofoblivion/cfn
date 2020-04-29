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
  { ctx with clang = path }

let with_stdlib path ctx =
  { ctx with stdlib = path }

let with_home path ctx =
  { ctx with home = path }

let with_root path ctx =
  { ctx with root = path }

let from_env ctx =
  ctx |> with_stdlib (Sys.getenv "CFNROOT")
      |> with_home (Sys.getenv "HOME")
      |> with_clang (Os.which "clang")
      |> with_root (Os.find_in_path "project.json" "" (Sys.getcwd ()))

(* Path Helpers *)

let build_dir _ = ""
let src_dir _ _ = ""
let lib_dir _ _ = ""
let pkg_dir _ = ""
