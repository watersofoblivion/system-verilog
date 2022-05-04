open Cmdliner

let cmd : unit Cmd.t =
  let doc = "Place and route" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;

    (*
     * FPGA Placement: Simulated Annealing? Min-cut?
     * FPGA Routing: PathFinder?
     * ASIC Routing: ePlace?
     *)
    `P ("Place and route")
  ] in

  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "pnr" ~doc ~sdocs ~exits:Common.exits ~man in

  Cmd.v info term
