open Cmdliner

(* 'fmt' -- Reformat package sources *)
let cmd =
  let doc = "Reformat package sources" in
  let man = [`S Manpage.s_description] in

  let fmt import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const fmt $ Common.import_path) in
  let info = Term.info "fmt" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
