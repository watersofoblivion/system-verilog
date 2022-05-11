%{
  [@@@coverage exclude_file]

  let make_incl_path (start_pos, end_pos) sys path env kontinue =
    let loc = Loc.loc start_pos end_pos in
    path env (fun env path ->
      Post.incl_path loc sys path
        |> kontinue env)

  let make_incl_dir (start_pos, end_pos) name args env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      optional args env (fun env args ->
        Post.incl_macro loc name args
          |> kontinue env))
%}

/* Test Entry Points */

%type <Env.env -> (Env.env -> Post.incl -> 'a) -> 'a> incl_test
%start incl_test

%%

/* Test Entry Points */

incl_test:
| src = incl; EOF { src }

/* Main Grammar */

%public incl:
| path = lit_str                         { make_incl_path $sloc false path }
| name = macro_name; args = option(args) { make_incl_dir $sloc name args }
