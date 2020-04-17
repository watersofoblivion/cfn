%{
  let make_package_stmt kwd_loc (name_loc, pkg_name) =
    Ast.package_stmt kwd_loc name_loc pkg_name

  let make_from_clause kwd_loc (ip_loc, import_path) =
    Ast.from_clause kwd_loc ip_loc import_path

  let make_package_alias (loc, alias) =
    Ast.package_alias loc alias

  let make_package_clause (pp_loc, package_path) alias =
    Ast.package_clause pp_loc package_path alias

  let make_import_clause kwd_loc packages =
    Ast.import_clause kwd_loc packages

  let make_import_stmt from_clause import_clause =
    Ast.import_stmt from_clause import_clause

  let make_file pkg imports =
    Ast.file pkg imports
%}

%token <Loc.t> EOF
%token <Loc.t> PACKAGE
%token <Loc.t> FROM IMPORT
%token <Loc.t> PIPE ARROW
%token <Loc.t * string> LIDENT
%token <Loc.t * string> STRING

%type <Ast.file> package_only imports_only file

%start package_only
%start imports_only
%start file

%%

/*
 * Source Files
 */

package_only:
    package_stmt end_of_package_only { make_file $1 [] }
;

end_of_package_only:
    FROM                { () }
  | IMPORT              { () }
  | end_of_imports_only { () }
;

imports_only:
    package_stmt import_stmt_list end_of_imports_only { make_file $1 $2 }
;

end_of_imports_only:
    end_of_file { () }
;

file:
    package_stmt import_stmt_list end_of_file { make_file $1 $2 }
;

end_of_file:
    EOF { () }
;

/*
 * Package Statement
 */

package_stmt:
    PACKAGE LIDENT { make_package_stmt $1 $2 }
;

/*
 * Imports
 */

import_stmt_list:
                                 { [] }
  | import_stmt import_stmt_list { $1::$2 }
;

import_stmt:
    from_clause import_clause { make_import_stmt $1 $2 }
;

from_clause:
                { None }
  | FROM STRING { Some (make_from_clause $1 $2) }
;

import_clause:
    IMPORT package_clause_list { make_import_clause $1 $2 }
;

package_clause_list:
    package_clause package_clause_list_tl { $1::$2 }
  | package_clause_list_tl                {     $1 }
;

package_clause_list_tl:
                                               { [] }
  | PIPE package_clause package_clause_list_tl { $2::$3 }
;

package_clause:
    STRING package_alias { make_package_clause $1 $2 }
;

package_alias:
                 { None }
  | ARROW LIDENT { Some (make_package_alias $2) }
;
