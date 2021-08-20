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
%token <Loc.t> PACKAGE "package"
%token <Loc.t> FROM "from"
%token <Loc.t> IMPORT "import"
%token <Loc.t> PIPE "|"
%token <Loc.t> ARROW "->"
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
| pkg_stmt = package_stmt; end_of_package_only { make_file pkg_stmt [] }

end_of_package_only:
| "from"              { () }
| "import"            { () }
| end_of_imports_only { () }

imports_only:
| pkg_stmt = package_stmt; import_stmts = import_stmt_list; end_of_imports_only { make_file pkg_stmt import_stmts }

end_of_imports_only:
| end_of_file { () }

file:
| pkg_stmt = package_stmt; import_stmts = import_stmt_list; end_of_file { make_file pkg_stmt import_stmts }

end_of_file:
| EOF { () }

/*
 * Package Statement
 */

package_stmt:
| stmt_loc = "package"; id = LIDENT { make_package_stmt stmt_loc id }

/*
 * Imports
 */

import_stmt_list:
|                                             { [] }
| stmt = import_stmt; rest = import_stmt_list { stmt :: rest }

import_stmt:
| from = from_clause; import = import_clause { make_import_stmt from import }

from_clause:
|                                 { None }
| from_loc = "from"; pkg = STRING { Some (make_from_clause from_loc pkg) }

import_clause:
| import_loc = "import"; clauses = package_clause_list { make_import_clause import_loc clauses }

package_clause_list:
| hd = package_clause; tl = package_clause_list_tl { hd :: tl }
| tl = package_clause_list_tl                      { tl }

package_clause_list_tl:
|                                                       { [] }
| "|"; hd = package_clause; tl = package_clause_list_tl { hd :: tl }

package_clause:
| pkg = STRING; alias = package_alias { make_package_clause pkg alias }

package_alias:
|                      { None }
| "->"; alias = LIDENT { Some (make_package_alias alias) }
