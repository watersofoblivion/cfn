open Cmdliner

(* 'pkg' -- Package build for deployment to AWS *)
let cmd =
  let doc = "Package build for deployment to AWS" in
  let man = [`S Manpage.s_description] in

  let pkg import_path =
    let _ = import_path in
    ()
  in

  let import_path =
    let doc = "Import path to build" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)
  in

  let term = Term.(const pkg $ import_path) in
  let info =
    Term.info "pkg" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man
  in

  (term, info)
