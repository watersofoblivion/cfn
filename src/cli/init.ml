open Cmdliner

(* 'init' -- Initialize a new project *)
let cmd =
  let doc = "Initialize a new project" in
  let man = [`S Manpage.s_description] in

  let init import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const init $ Common.import_path) in
  let info = Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
