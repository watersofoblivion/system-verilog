%{
  [@@@coverage exclude_file]
%}

%type <unit> file
%start file

%%

%public file:
| EOF { () }
