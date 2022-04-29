let _ =
  Cmdliner.Term.(exit @@ eval_choice Cli.cmd_default Cli.cmds)
