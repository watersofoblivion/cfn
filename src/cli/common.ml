open Cmdliner

(* Common Values *)

let exits = Term.default_exits

let import_path =
  let doc = "Import path to fetch" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"IMPORT_PATH" ~doc)
