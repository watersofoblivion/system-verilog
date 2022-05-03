open Cmdliner

let cmd : unit Cmd.t =
  let doc = "Prove a design using formal methods" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("Prove a design using formal methods")
  ] in

  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "prove" ~doc ~sdocs ~exits:Common.exits ~man in

  Cmd.v info term
