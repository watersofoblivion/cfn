open Cmdliner

(* 'fmt' -- Reformat package sources *)
let cmd =
  let doc = "Reformat package sources" in
  let man = [`S Manpage.s_description] in

  let fmt import_path =
    let _ = import_path in
    ()
  in

  let import_path =
    let doc = "Import path to format" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)
  in

  let term = Term.(const fmt $ import_path) in
  let info = Term.info "fmt" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
