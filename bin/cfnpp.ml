open Cmdliner

let pkg =
  let pkg import_path =
    let _ = import_path in
    ()
  in

  let import_path =
    let doc = "Import path to package" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)
  in

  let term = Term.(const pkg $ import_path) in
  let info = Term.info "pkg" in

  (term, info)

let _ =
  Term.exit @@ Term.eval pkg
