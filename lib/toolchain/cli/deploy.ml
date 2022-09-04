open Cmdliner

let impl import_path =
  let _ = import_path in
  ()

(* 'deploy' -- Deploy a package to AWS *)
let cmd =
  let doc = "Deploy a package to AWS" in
  let man = [`S Manpage.s_description] in

  let import_path =
    let doc = "Import path to deploy" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)
  in

  let term = Term.(const impl $ import_path) in
  let info =
    Cmd.info "deploy" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man
  in

  (term, info)
