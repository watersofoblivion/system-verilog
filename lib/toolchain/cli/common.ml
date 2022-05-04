open Cmdliner

(* Common Values *)

let help topic brief man =
  let term = Term.(ret (const (fun _ -> `Help (`Pager, Some topic)) $ const ())) in
  let info = Cmd.info topic ~doc:brief ~docs:"HELP TOPICS" ~man in
  Cmd.v info term

let exits = Cmd.Exit.defaults

let verbose =
  let doc = "Verbose" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

(* Compilation Units *)

let args_system_verilog_sources =
  (* Arg.(non_empty & pos_all file [] & info [] ~docv:"SYSTEM_VERILOG_FILES") *)
  Arg.(value & pos_all file [] & info [] ~docv:"SYSTEM_VERILOG_FILES")

type compilation_unit_mode =
  | CompilationUnitModeArgs
  | CompilationUnitModeFile

let compilation_unit_mode_conv =
  Arg.enum [("args", CompilationUnitModeArgs);
            ("file", CompilationUnitModeFile)]

let flag_compilation_unit_mode =
  Arg.(value & opt compilation_unit_mode_conv CompilationUnitModeFile & info ["compilation-unit-mode"])
