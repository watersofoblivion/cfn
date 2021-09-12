open Cmdliner

(* Common Values *)

let help topic brief man =
  let term = Term.(ret (const (fun _ -> `Help (`Pager, Some topic)) $ const ())) in
  let info = Term.info topic ~doc:brief ~docs:"HELP TOPICS" ~man in
  (term, info)

let exits = Term.default_exits

let verbose =
  let doc = "Verbose" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)
