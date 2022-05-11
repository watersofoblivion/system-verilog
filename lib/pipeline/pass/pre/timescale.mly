%{
  [@@@coverage exclude_file]

  let make_scale (start_pos, end_pos) mag yoonit env kontinue =
    let loc = Loc.loc start_pos end_pos in
    mag env (fun env mag ->
      yoonit env (fun env yoonit ->
        Post.scale loc mag yoonit
          |> kontinue env))
%}

%type <Env.env -> (Env.env -> Post.scale -> 'a) -> 'a> scale_test
%start scale_test

%%

/* Test Entry Points */

scale_test:
| scale = scale; EOF { scale }

/* Entry Points */

%public prec:
| "/"; prec = scale; { prec }

%public scale:
| mag = lit_num; yoonit = yoonit { make_scale $sloc mag yoonit }

%inline yoonit:
| yoonit = IDENT { make_value $sloc yoonit }
