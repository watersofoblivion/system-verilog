%{
  [@@@coverage exclude_file]

  let make_value (start_pos, end_pos) value env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Post.value loc value
      |> kontinue env
%}

%type <Env.env -> (Env.env -> Post.value -> 'a) -> 'a> lit_num_test
%type <Env.env -> (Env.env -> Post.value -> 'a) -> 'a> lit_str_test
%start lit_num_test
%start lit_str_test

%%

/* Test Entry Points */

lit_num_test:
| lit = lit_num; EOF { lit }

lit_str_test:
| lit = lit_str; EOF { lit }

/* Entry Points */

%public lit_num:
| lit = NUMBER { make_value $sloc lit }

%public lit_str:
| lit = STRING { make_value $sloc lit }
