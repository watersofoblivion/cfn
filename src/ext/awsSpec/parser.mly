%{
  open Path
%}

%token EOF LBRACKET RBRACKET DOT OR
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
    EOF                                  { blank }
  | json_path_expr EOF                   { $1 }
;

json_path_expr:
    IDENT                                { ident $1 }
  | json_path_expr DOT IDENT             { field $1 $3 }
  | json_path_expr LBRACKET IDX RBRACKET { index $1 $3 }
  | json_path_expr OR json_path_expr     { choice $1 $3 }
;

shape_path:
    EOF                 { blank }
  | shape_path_expr EOF { $1 }
;

shape_path_expr:
    IDENT                     { ident $1 }
  | shape_path_expr DOT IDENT { field $1 $3 }
;
