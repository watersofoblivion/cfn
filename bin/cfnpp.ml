open Cli

let cmds =
  [ (* Main commands *)
    Init.cmd;
    Get.cmd;
    Fmt.cmd;
    Build.cmd;
    Pkg.cmd;
    Deploy.cmd;

    (* Additional Help Topics *)
    Paths.cmd;
    Workspaces.cmd ]

let _ = Cmdliner.Term.(exit @@ eval_choice Default.cmd cmds)
