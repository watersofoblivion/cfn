(* Command line interface *)

open Cmdliner

(* Common Values *)

let exits = Term.default_exits

let import_path =
  let doc = "Import path to fetch" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)

(* 'get' -- Download and install packages and dependencies *)
let get_cmd =
  let doc = "Download and install packages and dependencies" in
  let man = [`S Manpage.s_description] in

  let get import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const get $ import_path) in
  let info = Term.info "get" ~doc ~sdocs:Manpage.s_common_options ~exits ~man in

  (term, info)

(* 'fmt' -- Reformat package sources *)
let fmt_cmd =
  let doc = "Reformat package sources" in
  let man = [`S Manpage.s_description] in

  let fmt import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const fmt $ import_path) in
  let info = Term.info "fmt" ~doc ~sdocs:Manpage.s_common_options ~exits ~man in

  (term, info)

(* 'build' -- Compile packages and dependencies *)
let build_cmd =
  let doc = "Compile packages and dependencies" in
  let man = [`S Manpage.s_description] in

  let build import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const build $ import_path) in
  let info =
    Term.info "build" ~doc ~sdocs:Manpage.s_common_options ~exits ~man
  in

  (term, info)

(* 'package' -- Package build for deployment to AWS *)
let package_cmd =
  let doc = "Package build for deployment to AWS" in
  let man = [`S Manpage.s_description] in

  let package import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const package $ import_path) in
  let info =
    Term.info "package" ~doc ~sdocs:Manpage.s_common_options ~exits ~man
  in

  (term, info)

(* 'deploy' -- Deploy a package to AWS *)
let deploy_cmd =
  let doc = "Deploy a package to AWS" in
  let man = [`S Manpage.s_description] in

  let deploy import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const deploy $ import_path) in
  let info =
    Term.info "deploy" ~doc ~sdocs:Manpage.s_common_options ~exits ~man
  in

  (term, info)

(* Default command -- Show help documentation *)
let default_cmd =
  let doc = "" in
  let sdocs = Manpage.s_common_options in
  let man = [`S Manpage.s_description] in

  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Term.info "cfn++" ~doc ~sdocs ~exits ~man in

  (term, info)

let cmds = [get_cmd; fmt_cmd; build_cmd; package_cmd; deploy_cmd]
