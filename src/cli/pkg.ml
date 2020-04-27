open Cmdliner

(* 'pkg' -- Package build for deployment to AWS *)
let cmd =
  let doc = "Package build for deployment to AWS" in
  let man = [`S Manpage.s_description] in

  let pkg import_path =
    let _ = import_path in
    ()
  in

  let term = Term.(const pkg $ import_path) in
  let info =
    Term.info "pkg" ~doc ~sdocs:Manpage.s_common_options ~exits ~man
  in

  (term, info)
