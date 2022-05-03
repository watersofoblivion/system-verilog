open Cmdliner

let cmd : unit Cmd.t =
  let doc = "Run an cycle-based simulation of a design" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("Run an cycle-based simulation of a design")
  ] in

  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "csim" ~doc ~sdocs ~exits:Common.exits ~man in

  Cmd.v info term
