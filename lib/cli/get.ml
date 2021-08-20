open Cmdliner

let impl update prerelease project_paths =
  let _ = update in
  let _ = prerelease in
  let _ = project_paths in
  ()

let cmd =
  let doc = "Fetch and update project dependencies" in
  let man = [
   `S Manpage.s_description;

   `P ("With no PROJECT-PATHS specificed, fetches the project sources of all "
    ^ "of the dependencies of the current project (as specified in the lock "
    ^ "file) into the workspace cache.  If PROJECT-PATHS are given, only the "
    ^ "project paths listed are fetched.");

   `P ("When the $(b,--update) ($(b,-u)) option is given, the dependencies are "
    ^ "updated to the latest compatible version, excluding pre-release "
    ^ "versions.  If the $(b,--pre-release) ($(b,-p)) option is given, "
    ^ "pre-release versions are included.");

   `S "VERSIONING";

   `P ("Version compatibility is determined by semantic versioning, and all "
    ^ "major versions of a project are considered backwards-compatible.  When "
    ^ "fetching dependencies, for each dependency that appears multiple times "
    ^ "in the project and/or its dependencies the most recent capatible "
    ^ "version listed in the project or any dependency is chosen.  When "
    ^ "updating the most recent compatible version found is chosen.");

   `P ("Project versions are determined by the scheme below.  Note that each "
    ^ "major version can use either the tagging or branching exclusively, but "
    ^ "different strategies can be chosen for different major versions.");

   `P ("First, if tags named $(b,v<SEMVER>) (such as $(i,v1.2.3), "
    ^ "$(i,v2.2.9-beta), etc.) are present, they are considered the only "
    ^ "available versions to choose from.");

   `P ("Next, if branches of named $(b,v<MAJOR>) (such as $(i,v2), $(i,v3)) "
    ^ "are present, the HEAD of the branch is treated as version "
    ^ "$(b,<MAJOR>.0.0-<COMMIT-TIMESTAMP>.<COMMIT-SHA>) and is treated as a "
    ^ "released version.  For example, the HEAD of the $(i,v2) branch could be "
    ^ "version "
    ^ "$(i,2.0.0-20200704010203.b06c0be41dc4866aff8f9dfa8d66a21682ac97c8).");

   `P ("Finally, if neither tags nor branches are found, the main branch "
    ^ "(usually $(i,master)) is considered to be the $(i,v0) branch and is "
    ^ "treated as above.")
  ] in

  let update =
    let doc = "Update projects" in
    Arg.(value & flag & info ["u"; "update"] ~doc)
  in
  let prerelease =
    let doc = "Allow pre-release versions for the listed projects" in
    Arg.(value & flag & info ["p"; "pre-release"] ~doc)
  in

  let project_paths =
    let doc = "Project path(s) to get" in
    Arg.(value & pos_all string [] & info [] ~docv:"PROJECT-PATHS" ~doc)
  in

  let term = Term.(const impl $ update $ prerelease $ project_paths) in
  let info = Term.info "get" ~doc ~sdocs:Manpage.s_common_options ~exits:Common.exits ~man in

  (term, info)
