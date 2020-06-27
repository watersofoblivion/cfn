open Cmdliner

(* 'get' -- Download and install packages and dependencies *)
let cmd =
  let doc = "Download and install packages and dependencies" in
  let man = [`S Manpage.s_description] in

  let get update import_path =
    let _ = update in
    let _ = import_path in
    ()
  in

  let update =
    let doc = "Update packages" in
    Arg.(value & flag & info ["u"; "update"] ~doc)
  in

  let term = Term.(const get $ update $ Common.import_path) in
  let info = Term.info "get" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
