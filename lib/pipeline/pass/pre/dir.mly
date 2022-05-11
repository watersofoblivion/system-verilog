%{
  [@@@coverage exclude_file]

  let make_dir_reset_all (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_reset_all
      |> kontinue env

  let make_dir_include (start_pos, end_pos) src env kontinue =
    let loc = Loc.loc start_pos end_pos in
    src env (fun env src ->
      Post.dir_include loc src
        |> kontinue env)

  let make_dir_define (start_pos, end_pos) name params body env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      optional params env (fun env params ->
        optional body env (fun env body ->
          Post.dir_define loc name params body
            |> kontinue env)))

  let make_dir_undef (start_pos, end_pos) name env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      Post.dir_undef loc name
        |> kontinue env)

  let make_dir_undefine_all (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_undefine_all
      |> kontinue env

  let make_dir_macro (start_pos, end_pos) name args env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      optional args env (fun env args ->
        Post.dir_macro loc name args
          |> kontinue env))

  let make_dir_if_def (start_pos, end_pos) macro env kontinue =
    let loc = Loc.loc start_pos end_pos in
    macro env (fun env macro ->
      Post.dir_if_def loc macro
        |> kontinue env)

  let make_dir_if_n_def (start_pos, end_pos) macro env kontinue =
    let loc = Loc.loc start_pos end_pos in
    macro env (fun env macro ->
      Post.dir_if_n_def loc macro
        |> kontinue env)

  let make_dir_els_if (start_pos, end_pos) macro env kontinue =
    let loc = Loc.loc start_pos end_pos in
    macro env (fun env macro ->
      Post.dir_els_if loc macro
        |> kontinue env)

  let make_dir_else (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_else
      |> kontinue env

  let make_dir_end_if (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_end_if
      |> kontinue env

  let make_dir_timescale (start_pos, end_pos) yoonit prec env kontinue =
    let loc = Loc.loc start_pos end_pos in
    yoonit env (fun env yoonit ->
      optional prec env (fun env prec ->
        Post.dir_timescale loc yoonit prec
          |> kontinue env))

  let make_dir_default_net_type (start_pos, end_pos) net env kontinue =
    let loc = Loc.loc start_pos end_pos in
    net env (fun env net ->
      Post.dir_default_net_type loc net
        |> kontinue env)

  let make_dir_unconnected_drive (start_pos, end_pos) drive env kontinue =
    let loc = Loc.loc start_pos end_pos in
    drive env (fun env drive ->
      Post.dir_unconnected_drive loc drive
        |> kontinue env)

  let make_dir_no_unconnected_drive (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_no_unconnected_drive
      |> kontinue env

  let make_dir_cell_define (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_cell_define
      |> kontinue env

  let make_dir_end_cell_define (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_end_cell_define
      |> kontinue env

  let make_dir_pragma (start_pos, end_pos) name exprs env kontinue =
    let loc = Loc.loc start_pos end_pos in
    name env (fun env name ->
      map exprs env (fun env exprs ->
        Post.dir_pragma loc name exprs
          |> kontinue env))

  let make_dir_line (start_pos, end_pos) number path level env kontinue =
    let loc = Loc.loc start_pos end_pos in
    number env (fun env number ->
      path env (fun env path ->
        level env (fun env level ->
          Post.dir_line loc number path level
            |> kontinue env)))

  let make_dir_FILE (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_FILE
      |> kontinue env

  let make_dir_LINE (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_LINE
      |> kontinue env

  let make_dir_begin_keywords (start_pos, end_pos) keywords env kontinue =
    let loc = Loc.loc start_pos end_pos in
    keywords env (fun env keywords ->
      Post.dir_begin_keywords loc keywords
        |> kontinue env)

  let make_dir_end_keywords (start_pos, end_pos) env kontinue =
    Loc.loc start_pos end_pos
      |> Post.dir_end_keywords
      |> kontinue env
%}

/* Test Entry Points */

%type <Env.env -> (Env.env -> Post.dir -> 'a) -> 'a> dir_test
%start dir_test

%%

/* Test Entry Points */

dir_test:
| dir = dir; EOF { dir }

/* Main Grammar */

%public dir:
| dir = dir_reset_all         { dir }
| dir = dir_include           { dir }
| dir = dir_macro             { dir }
| dir = dir_cond              { dir }
| dir = dir_timescale         { dir }
| dir = dir_default_net_type  { dir }
| dir = dir_unconnected_drive { dir }
| dir = dir_cell_define       { dir }
| dir = dir_pragma            { dir }
| dir = dir_line              { dir }
| dir = dir_loc               { dir }
| dir = dir_keywords          { dir }

%inline %public dir_reset_all:
| "`resetall" { make_dir_reset_all $sloc }

%inline %public dir_include:
| "`include"; src = incl { make_dir_include $sloc src }

%inline %public dir_macro:
| "`define"; macro = ident; params = option(params); body = option(body) { make_dir_define $sloc macro params body }
| "`undef"; macro = ident                                                { make_dir_undef $sloc macro }
| "`undefineall"                                                         { make_dir_undefine_all $sloc }
| name = macro_name; args = option(args)                                 { make_dir_macro $sloc name args }

%inline %public macro_name:
| name = "`macro_name" { make_ident $sloc name }

%inline %public dir_cond:
| "`ifdef"; macro = ident  { make_dir_if_def $sloc macro }
| "`ifndef"; macro = ident { make_dir_if_n_def $sloc macro }
| "`elsif"; macro = ident  { make_dir_els_if $sloc macro }
| "`else"                  { make_dir_else $sloc }
| "`endif"                 { make_dir_end_if $sloc }

%inline %public dir_timescale:
| "`timescale"; yoonit = scale; prec = option(prec) { make_dir_timescale $sloc yoonit prec }

%inline %public dir_default_net_type:
| "`default_nettype"; net = ident { make_dir_default_net_type $sloc net }

%inline %public dir_unconnected_drive:
| "`unconnected_drive"; drive = ident { make_dir_unconnected_drive $sloc drive }
| "`nounconnected_drive"              { make_dir_no_unconnected_drive $sloc }

%inline %public dir_cell_define:
| "`celldefine"    { make_dir_cell_define $sloc }
| "`endcelldefine" { make_dir_end_cell_define $sloc }

%inline %public dir_pragma:
| "`pragma"; name = ident; exprs = separated_list(",", pragma_expr) { make_dir_pragma $sloc name exprs }

%inline %public dir_line:
| "`line"; num = lit_num; path = lit_str; level = lit_num { make_dir_line $sloc num path level }

%inline %public dir_loc:
| "`__FILE__" { make_dir_FILE $sloc }
| "`__LINE__" { make_dir_LINE $sloc }

%inline %public dir_keywords:
| "`begin_keywords"; keywords = lit_str { make_dir_begin_keywords $sloc keywords }
| "`end_keywords"                       { make_dir_end_keywords $sloc }
