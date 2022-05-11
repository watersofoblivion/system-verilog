%{
  [@@@coverage exclude_file]

  let make_seg_source (start_pos, end_pos) src env kontinue =
    let loc = Loc.loc start_pos end_pos in
    Post.seg_source loc src
      |> kontinue env
%}

%type <Env.env -> (Env.env -> Post.seg -> 'a) -> 'a> seg_test
%start seg_test

%%

/* Test Entry Points */

seg_test:
| seg = segment; EOF { seg }

/* Main Grammar */

%public segment:
| src = SOURCE { make_seg_source $sloc src }
