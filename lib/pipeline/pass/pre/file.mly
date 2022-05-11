%{
  [@@@coverage exclude_file]

  open Common

  let make_file segs env kontinue =
    map segs env (fun env segs ->
      Post.file (Result.get_ok (Fpath.of_string "/")) segs
        |> kontinue env)
%}

/* Main Grammar */

%type <Env.env -> (Env.env -> Post.file -> 'a) -> 'a> file
%start file

%%

/* Main Grammar */

%public file:
| segs = list(segment); EOF { make_file segs }
