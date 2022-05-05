open Cmdliner

let cmd : unit Cmd.t =
  let doc = "Document a design" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    `P ("Document a design")
  ] in

  let gen = () in

  let term = Term.(const gen) in
  let info = Cmd.info "doc" ~doc ~sdocs ~exits:Common.exits ~man in

  Cmd.v info term
