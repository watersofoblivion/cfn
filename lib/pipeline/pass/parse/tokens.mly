/*
 * Tokens
 */

/* Non-Printable */
%token EOF
%token WHITESPACE

/* Punctuation */
%token LPAREN "("
%token RPAREN ")"
%token PIPE "|"
%token ARROW "->"
%token COLON ":"
%token BIND "="
%token GROUND "_"
%token SQUOTE "'"
%token DQUOTE
%token PROTO "://"
%token DOT "."
%token PATH_SEP
%token AT "@"
%token V "v"

/* Operators */
%token UN_NEG "-"
%token UN_LOG_NOT "!"
%token UN_BIT_NOT "~"

%token BIN_STRUCT_EQ "=="
%token BIN_STRUCT_NEQ "!="
%token BIN_PHYS_EQ "==="
%token BIN_PHYS_NEQ "!=="
%token BIN_LT "<"
%token BIN_LTE "<="
%token BIN_GT ">"
%token BIN_GTE ">="
%token BIN_LSL "<<"
%token BIN_LSR ">>"
%token BIN_ASL "<<<"
%token BIN_ASR ">>>"
%token BIN_ADD "+"
%token BIN_MUL "*"
%token BIN_DIV "/"
%token BIN_MOD "%"
%token BIN_EXP "^^"
%token BIN_LOG_AND "&&"
%token BIN_LOG_OR "||"
%token BIN_BIT_AND "&"
%token BIN_BIT_XOR "^"

/* Keywords */
%token PACKAGE "package"
%token FROM "from"
%token IMPORT "import"
%token LET "let"
%token IN "in"
%token VAL "val"

/* Import Paths */
%token <string> HOST PATH_SEG VERSION

/* Literals */
%token <bool> BOOL
%token <string> INT LONG FLOAT DOUBLE

/* Unicode */
%token <string> UESC

/* Runes */
%token <Uchar.t> RUNE

/* Strings */
%token NEWLINE
%token <string> STRING

/* Identifiers */
%token <string> UIDENT LIDENT

%%
