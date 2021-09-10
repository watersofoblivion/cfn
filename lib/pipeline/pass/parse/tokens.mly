/*
 * Tokens
 */

/* Non-Printable */
%token EOF

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
