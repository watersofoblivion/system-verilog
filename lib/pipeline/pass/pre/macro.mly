%{
  [@@@coverage exclude_file]

  let make_param (start_pos, end_pos) name default env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      optional default env (fun env default ->
        Post.param loc name default
          |> kontinue env))

  let make_params (start_pos, end_pos) params env kontinue =
    let loc = Loc.loc start_pos end_pos in
    map params env (fun env params ->
      Post.params loc params
        |> kontinue env)

  let make_elem_source (start_pos, end_pos) src env kontinue =
    let loc = Loc.loc start_pos end_pos in
    src env (fun env src ->
      Post.elem_source loc src
        |> kontinue env)

  let make_elem_var (start_pos, end_pos) name env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      Post.elem_var loc name
        |> kontinue env)

  let make_line (start_pos, end_pos) elems env kontinue =
    let loc = Loc.loc start_pos end_pos in
    map elems env (fun env elems ->
      Post.line loc elems
        |> kontinue env)

  let make_body (start_pos, end_pos) lines env kontinue =
    let loc = Loc.loc start_pos end_pos in
    map lines env (fun env lines ->
      Post.body loc lines
        |> kontinue env)

  let make_args (start_pos, end_pos) args env kontinue =
    let loc = Loc.loc start_pos end_pos in
    map args env (fun env args ->
      Post.args loc args
        |> kontinue env)
%}

%type <Env.env -> (Env.env -> Post.params -> 'a) -> 'a> params_test
%type <Env.env -> (Env.env -> Post.param -> 'a) -> 'a> param_test
%type <Env.env -> (Env.env -> Post.body -> 'a) -> 'a> body_test
%type <Env.env -> (Env.env -> Post.line -> 'a) -> 'a> line_test
%type <Env.env -> (Env.env -> Post.elem -> 'a) -> 'a> elem_test
%type <Env.env -> (Env.env -> Post.args -> 'a) -> 'a> args_test
%start params_test
%start param_test
%start body_test
%start line_test
%start elem_test
%start args_test

%%

/* Test Entry Points */

params_test:
| params = params; EOF { params }

param_test:
| param = param; EOF { param }

body_test:
| body = body; EOF { body }

line_test:
| line = line; EOF { line }

elem_test:
| elem = elem; EOF { elem }

args_test:
| args = args; EOF { args }

/* Entry Points */

%public params:
| "("; params = separated_list(",", param); ")" { make_params $sloc params }

%public param:
| name = ident; default = option(preceded("=", default)) { make_param $sloc name default }

%public body:
| lines = separated_nonempty_list(LINE_SEP, line) { make_body $sloc lines }

%public line:
| elems = nonempty_list(elem) { make_line $sloc elems }

%public elem:
| src = source { make_elem_source $sloc src }
| id = ident   { make_elem_var $sloc id }

%inline source:
| src = SOURCE { make_value $sloc src }

%public args:
| "("; args = separated_list(",", default); ")" { make_args $sloc args }

%inline default:
| text = DEFAULT { make_value $sloc text }
