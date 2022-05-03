open Cmdliner

let term =
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ()))

let cmd : unit Cmd.t =
  let doc = "Help" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("$(mname) is a IEEE 1800-2012 compliant implementation of System Verilog")
  ] in

  let info = Cmd.info "help" ~doc ~sdocs ~exits:Common.exits ~man in

  Cmd.v info term
