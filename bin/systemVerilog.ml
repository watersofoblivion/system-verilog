let _ =
  Cmdliner.Cmd.(Stdlib.exit @@ eval Cli.cmds)
