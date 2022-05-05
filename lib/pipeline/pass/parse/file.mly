%{
  [@@@coverage exclude_file]

  let rec make_tops tops env kontinue = match tops with
    | [] -> kontinue env []
    | top :: tops ->
      top env (fun env top ->
        make_tops tops env (fun env tops ->
          top :: tops
            |> kontinue env))

  let make_file tops env kontinue =
    make_tops tops env (fun env tops ->
      tops
        |> Syntax.file
        |> kontinue env)
%}

%type <Env.env -> (Env.env -> Syntax.file -> 'a) -> 'a> file
%start file

%%

%public file:
| EOF { make_file [] }
