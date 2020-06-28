open Cmdliner

open Pipeline

(* 'init' -- Initialize a new project *)
let cmd =
  let doc = "Initialize a new project" in
  let man = [`S Manpage.s_description] in

  let init import_path dir =
    let dir =
      if Filename.is_relative dir
      then
        let cwd = Sys.getcwd () in
        Filename.concat cwd dir
      else dir
    in
    let path =
      import_path
        |> Path.project
    in
    let _ = Workspace.create path dir in
    ()
  in

  let import_path =
    let doc = "Import path to fetch" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)
  in
  let dir =
    let doc = "Project directory" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in

  let term = Term.(const init $ import_path $ dir) in
  let info = Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
