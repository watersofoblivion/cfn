open Cmdliner

(* 'get' -- Download and install packages and dependencies *)
let cmd =
  let doc = "Download and install packages and dependencies" in
  let man = [`S Manpage.s_description] in

  let get import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const get $ import_path) in
  let info = Term.info "get" ~doc ~sdocs:Manpage.s_common_options ~exits ~man in

  (term, info)
