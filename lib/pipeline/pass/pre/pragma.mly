%{
  [@@@coverage exclude_file]

  open Common

  let make_pragma_value_exprs (start_pos, end_pos) exprs env kontinue =
    let loc = Loc.loc start_pos end_pos in
    map exprs env (fun env exprs ->
      Post.pragma_value_exprs loc exprs
        |> kontinue env)

  let make_pragma_value_num (start_pos, end_pos) num env kontinue =
    let loc = Loc.loc start_pos end_pos in
    num env (fun env num ->
      Post.pragma_value_num loc num
        |> kontinue env)

  let make_pragma_value_str (start_pos, end_pos) str env kontinue =
    let loc = Loc.loc start_pos end_pos in
    str env (fun env str ->
      Post.pragma_value_string loc str
        |> kontinue env)

  let make_pragma_value_ident (start_pos, end_pos) ident env kontinue =
    let loc = Loc.loc start_pos end_pos in
    ident env (fun env ident ->
      Post.pragma_value_ident loc ident
        |> kontinue env)

  let make_pragma_expr (start_pos, end_pos) kwd value env kontinue =
    let loc = Loc.loc start_pos end_pos in
    optional kwd env (fun env kwd ->
      optional value env (fun env value ->
        Post.pragma_expr loc kwd value
          |> kontinue env))
%}

%type <Env.env -> (Env.env -> Post.pragma_expr -> 'a) -> 'a> pragma_expr_test
%type <Env.env -> (Env.env -> Post.pragma_value -> 'a) -> 'a> pragma_value_test
%start pragma_expr_test
%start pragma_value_test

%%

/* Test Entry Points */

pragma_expr_test:
| expr = pragma_expr; EOF { expr }

pragma_value_test:
| value = pragma_value; EOF { value }

/* Entry Points */

%public pragma_expr:
| kwd = ident; value = option(preceded("=", ext_pragma_value)) { make_pragma_expr $sloc (Some kwd) value }
| value = pragma_value                                         { make_pragma_expr $sloc None (Some value) }

%public pragma_value:
| "("; exprs = separated_list(",", pragma_expr); ")" { make_pragma_value_exprs $sloc exprs }
| lit = lit_num                                      { make_pragma_value_num $sloc lit }
| lit = lit_str                                      { make_pragma_value_str $sloc lit }

ext_pragma_value:
| value = pragma_value { value }
| id = ident           { make_pragma_value_ident $sloc id }
