open Cmdliner

open Artifact

let impl id dir =
  let dir =
    if Filename.is_relative dir
    then
      let cwd = Sys.getcwd () in
      Filename.concat cwd dir
    else dir
  in
  let id = id |> Path.id in
  (* let _ = Workspace.create id dir in *)
  let _ = dir in
  let _ = id in
  ()

let cmd =
  let doc = "Initialize a new project" in
  let man = [
   `S Manpage.s_description;

   `P ("Generates a new empty CFN++ project named PROJECT-ID into DIRECTORY.  "
    ^ "The target directory must not already exist.")
  ] in

  let id =
    let doc = "Project ID" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT-ID" ~doc)
  in
  let dir =
    let doc = "Directory to generate the project in" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in

  let term = Term.(const impl $ id $ dir) in
  let info = Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
