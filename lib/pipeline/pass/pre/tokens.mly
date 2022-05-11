%{
  [@@@coverage exclude_file]
%}

/* Non-Printable */

%token EOF
%token NEWLINE

/* Punctuation */

%token PUNCT_DQUOTE
%token PUNCT_LANGLE "<"
%token PUNCT_RANGLE ">"
%token PUNCT_LPAREN "("
%token PUNCT_RPAREN ")"
%token PUNCT_LBRACKET "["
%token PUNCT_RBRACKET "]"
%token PUNCT_LBRACE "{"
%token PUNCT_RBRACE "}"
%token PUNCT_COMMA ","
%token PUNCT_EQ "="
%token PUNCT_SLASH "/"

/* Macros */

%token <string> DEFAULT
%token <string> LINE
%token LINE_SEP

/* Directives */

%token DIR_RESET_ALL "`resetall"
%token DIR_INCLUDE "`include"
%token DIR_DEFINE "`define"
%token DIR_UNDEF "`undef"
%token DIR_UNDEFINE_ALL "`undefineall"
%token <string> DIR_MACRO "`macro_name"
%token DIR_IF_DEF "`ifdef"
%token DIR_IF_N_DEF "`ifndef"
%token DIR_ELS_IF "`elsif"
%token DIR_ELSE "`else"
%token DIR_END_IF "`endif"
%token DIR_TIMESCALE "`timescale"
%token DIR_DEFAULT_NET_TYPE "`default_nettype"
%token DIR_UNCONNECTED_DRIVE "`unconnected_drive"
%token DIR_NO_UNCONNECTED_DRIVE "`nounconnected_drive"
%token DIR_CELL_DEFINE "`celldefine"
%token DIR_END_CELL_DEFINE "`endcelldefine"
%token DIR_PRAGMA "`pragma"
%token DIR_LINE_LEVEL "`line"
%token DIR_FILE "`__FILE__"
%token DIR_LINE "`__LINE__"
%token DIR_BEGIN_KEYWORDS "`begin_keywords"
%token DIR_END_KEYWORDS "`end_keywords"

/* General */

%token <string> IDENT
%token <string> NUMBER
%token <string> STRING

/* Source Code */

%token <string> SOURCE

%%
