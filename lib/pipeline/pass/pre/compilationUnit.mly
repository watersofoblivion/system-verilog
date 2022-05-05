%{
  [@@@coverage exclude_file]

  let make_compilation_unit files env kontinue =
    files
      |> Post.compilation_unit
      |> kontinue env
%}

%type <Env.env -> (Env.env -> Post.t -> 'a) -> 'a> compilation_unit
%start compilation_unit

%%

%public compilation_unit:
| EOF { make_compilation_unit [] }
