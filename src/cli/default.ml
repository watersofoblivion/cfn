open Cmdliner

(* Default command -- Show help documentation *)
let cmd =
  let doc = "" in
  let sdocs = Manpage.s_common_options in
  let man = [`S Manpage.s_description] in

  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Term.info "cfn++" ~doc ~sdocs ~exits ~man in

  (term, info)
