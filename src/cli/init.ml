open Cmdliner

open Pipeline

(* 'init' -- Initialize a new project *)
let cmd =
  let doc = "Initialize a new project" in
  let man = [`S Manpage.s_description] in

  let init id dir =
    let dir =
      if Filename.is_relative dir
      then
        let cwd = Sys.getcwd () in
        Filename.concat cwd dir
      else dir
    in
    let id = id |> Path.id in
    let _ = Workspace.create id dir in
    ()
  in

  let id =
    let doc = "Project ID" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)
  in
  let dir =
    let doc = "Project directory" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in

  let term = Term.(const init $ id $ dir) in
  let info = Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
