%{
  open Path
%}

%token EOF
%token LBRACKET "["
%token RBRACKET "]"
%token DOT "."
%token OR "|"
%token <int> IDX
%token <string> IDENT

%type <Path.t> json_path
%type <Path.t> shape_path

%left DOT
%left LBRACKET
%left OR

%start json_path
%start shape_path

%%

json_path:
    EOF                        { blank }
  | expr = json_path_expr; EOF { expr }
;

json_path_expr:
    id = IDENT                                        { ident id }
  | expr = json_path_expr; "."; id = IDENT            { field expr id }
  | expr = json_path_expr; "["; idx = IDX; "]"        { index expr idx }
  | expr1 = json_path_expr "|" expr2 = json_path_expr { choice expr1 expr2 }
;

shape_path:
    EOF                         { blank }
  | expr = shape_path_expr; EOF { expr }
;

shape_path_expr:
    id = IDENT                              { ident id }
  | expr = shape_path_expr; "."; id = IDENT { field expr id }
;
