open Cfn

let _ = Cmdliner.Term.(exit @@ eval_choice Cli.default_cmd Cli.cmds)
