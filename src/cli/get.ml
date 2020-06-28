open Cmdliner

(* 'get' -- Download and install packages and dependencies *)
let cmd =
  let doc = "Download and install packages and dependencies" in
  let man = [`S Manpage.s_description] in

  let get update prerelease import_path =
    let _ = update in
    let _ = prerelease in
    let _ = import_path in
    ()
  in

  let update =
    let doc = "Update packages" in
    Arg.(value & flag & info ["u"; "update"] ~doc)
  in
  let prerelease =
    let doc = "Allow pre-release packages" in
    Arg.(value & flag & info ["p"; "pre-release"] ~doc)
  in

  let import_paths =
    let doc = "Import path(s) to fetch" in
    Arg.(value & pos_all string [] & info [] ~docv:"IMPORT-PATHS" ~doc)
  in

  let term = Term.(const get $ update $ prerelease $ import_paths) in
  let info = Term.info "get" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
