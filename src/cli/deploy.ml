open Cmdliner

(* 'deploy' -- Deploy a package to AWS *)
let cmd =
  let doc = "Deploy a package to AWS" in
  let man = [`S Manpage.s_description] in

  let deploy import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const deploy $ import_path) in
  let info =
    Term.info "deploy" ~doc ~sdocs:Manpage.s_common_options ~exits ~man
  in

  (term, info)
