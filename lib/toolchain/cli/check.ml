open Cmdliner

let cmd : unit Cmd.t =
  let doc = "Syntax check a design" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("Syntax check a design")
  ] in

  let check mode sources =
    let printer = Arg.conv_printer Common.compilation_unit_mode_conv in
    Format.printf "Compiling sources in %a mode: %s\n"
      printer mode
      (String.concat ", " sources)
  in

  let term = Term.(const check $ Common.flag_compilation_unit_mode $ Common.args_system_verilog_sources) in
  let info = Cmd.info "check" ~doc ~sdocs ~exits:Common.exits ~man in

  Cmd.v info term
