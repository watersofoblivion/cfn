open Cli

let cmds =
  [ Init.cmd;
    Get.cmd;
    Fmt.cmd;
    Build.cmd;
    Pkg.cmd;
    Deploy.cmd ]

let _ = Cmdliner.Term.(exit @@ eval_choice Default.cmd cmds)
