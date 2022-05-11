%{
  [@@@coverage exclude_file]

  open Common

  let optional opt env kontinue =
    match opt with
      | None -> kontinue env None
      | Some opt ->
        opt env (fun env opt ->
          (Some opt)
            |> kontinue env)

  let rec map lst env kontinue = match lst with
    | [] -> kontinue env []
    | hd :: tl ->
      hd env (fun env hd ->
        map tl env (fun env tl ->
          hd :: tl
            |> kontinue env))
%}

%%
