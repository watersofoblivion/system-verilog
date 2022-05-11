%{
  [@@@coverage exclude_file]

  let make_ident (start_pos, end_pos) name env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Post.name loc name
      |> kontinue env
%}

%type <Env.env -> (Env.env -> Post.name -> 'a) -> 'a> ident_test
%start ident_test

%%

/* Test Entry Points */

ident_test:
| id = ident; EOF { id }

/* Entry Points */

%public ident:
| id = IDENT { make_ident $sloc id }
