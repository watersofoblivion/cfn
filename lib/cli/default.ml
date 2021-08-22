open Cmdliner

(* Default command -- Show help documentation *)
let cmd =
  let doc = "A native-code compiler for AWS::Serverless" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("$(mname) is a native-code compiler for AWS::Serverless.")
  ] in

  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Term.info "cfn++" ~doc ~sdocs ~exits:Common.exits ~man in

  (term, info)
