open Cmdliner

(* Common Values *)

let exits = Term.default_exits

let verbose =
  let doc = "Verbose" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)
