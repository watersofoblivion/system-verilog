open Cmdliner

include Help
include Elaborate

let cmds =
  let doc = "System Verilog" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("$(mname) is a IEEE 1800-2012 compliant implementation of System Verilog")
  ] in
  let info = Cmd.info "system-verilog" ~doc ~sdocs ~exits:Common.exits ~man in
  Cmd.group ~default:Help.term info [
    Help.cmd;
    Check.cmd;
    Elaborate.cmd;
    Esim.cmd;
    Csim.cmd;
    Prove.cmd;
  ]
