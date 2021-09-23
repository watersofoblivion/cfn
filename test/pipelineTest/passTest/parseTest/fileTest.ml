(* Source Files *)

open Format

open OUnit2

open Common

(* Fixtures *)

(* Lexemes *)

let pkgname_lexeme = "pkgname"

let stdlib_pkgpath_lexeme = "first/stdlib/pkg/path"
let stdlib_localname_lexeme = "stdliblocalname"
let stdlib_pkgpath_lexeme' = "second/stdlib/pkg/path"

let current_pkgpath_lexeme = "first/current/pkg/path"
let current_localname_lexeme = "currentlocalname"
let current_pkgpath_lexeme' = "second/current/pkg/path"

let proto_lexeme = "https"
let host_lexeme = "github.com"
let hostpath_lexeme = "owner/repo"
let version_lexeme = "42"

let external_pkgpath_lexeme = "first/external/pkg/path"
let external_localname_lexeme = "externallocalname"
let external_pkgpath_lexeme' = "second/external/pkg/path"

let let_bound_lexeme = "letBoundOne"
let let_type_lexeme = Prim.id_int
let let_value_lexeme = "42"

let let_bound_lexeme' = "letBoundTwo"
let let_value_lexeme' = "true"

let val_bound_lexeme = "valBoundOne"
let val_type_lexeme = Prim.id_float
let val_value_lexeme = "4.2"

let val_bound_lexeme' = "valBoundTwo"
let val_value_lexeme' = "\"foobar\""

(* Raw Source *)

let source = [
  sprintf "package %s" pkgname_lexeme;
          "";
          "import";
  sprintf "  | %S -> %s" stdlib_pkgpath_lexeme stdlib_localname_lexeme;
  sprintf "  | %S" stdlib_pkgpath_lexeme';
          "from . import";
  sprintf "  | %S -> %s" current_pkgpath_lexeme current_localname_lexeme;
  sprintf "  | %S" current_pkgpath_lexeme';
  sprintf "from %s://%s/%s@v%s import" proto_lexeme host_lexeme hostpath_lexeme version_lexeme;
  sprintf "  | %S -> %s" external_pkgpath_lexeme external_localname_lexeme;
  sprintf "  | %S" external_pkgpath_lexeme';
          "";
  sprintf "let %s: %s = %s" let_bound_lexeme let_type_lexeme let_value_lexeme;
  sprintf "let %s = %s" let_bound_lexeme' let_value_lexeme';
          "";
  sprintf "val %s: %s = %s" val_bound_lexeme val_type_lexeme val_value_lexeme;
  sprintf "val %s = %s" val_bound_lexeme' val_value_lexeme';
]

(* AST *)

let seq = Sym.seq ()

let pkg =
  let name =
    let id = Sym.gen seq ~id:pkgname_lexeme in
    ImportTest.fresh_name ~start:(1, 8, 8) ~id ()
  in
  ImportTest.fresh_pkg ~name ()

let stdlib_import =
  let aliases =
    let pkg = ImportTest.fresh_pkgpath ~start:(4, 4, 28) ~segs:["first"; "stdlib"; "pkg"; "path"] () in
    let alias =
      let id = Sym.gen seq ~id:stdlib_localname_lexeme in
      ImportTest.fresh_name ~start:(4, 31, 55) ~id ()
    in
    let pkg' = ImportTest.fresh_pkgpath ~start:(5, 4, 75) ~segs:["second"; "stdlib"; "pkg"; "path"] () in
    [
      ImportTest.fresh_alias ~pkg ~with_alias:true ~alias ();
      ImportTest.fresh_alias ~pkg:pkg' ~with_alias:false ()
    ]
  in
  let pkgs = ImportTest.fresh_pkgs ~start:(3, 0, 17) ~aliases () in
  ImportTest.fresh_import ~start:(3, 0, 17) ~stdlib:true ~pkgs ()

let current_import =
  let from =
    let src = ImportTest.fresh_src_current ~start:(6, 5, 105) () in
    ImportTest.fresh_from ~start:(6, 0, 100) ~src ()
  in
  let aliases =
    let pkg = ImportTest.fresh_pkgpath ~start:(7, 4, 118) ~segs:["first"; "current"; "pkg"; "path"] () in
    let alias =
      let id = Sym.gen seq ~id:current_localname_lexeme in
      ImportTest.fresh_name ~start:(7, 32, 146) ~id ()
    in
    let pkg' = ImportTest.fresh_pkgpath ~start:(8, 4, 167) ~segs:["second"; "current"; "pkg"; "path"] () in
    [
      ImportTest.fresh_alias ~pkg ~with_alias:true ~alias ();
      ImportTest.fresh_alias ~pkg:pkg' ~with_alias:false ()
    ]
  in
  let pkgs = ImportTest.fresh_pkgs ~start:(6, 7, 107) ~aliases () in
  ImportTest.fresh_import ~start:(6, 0, 103) ~stdlib:false ~from ~pkgs ()

let external_import =
  let from =
    let src =
      let proto = ImportTest.fresh_proto ~start:(9, 5, 198) ~proto:proto_lexeme () in
      let host = ImportTest.fresh_host ~start:(9, 13, 206) ~host:host_lexeme () in
      let path = ImportTest.fresh_hostpath ~start:(9, 23, 216) ~segs:["owner"; "repo"] () in
      let version = ImportTest.fresh_version ~start:(9, 34, 227) ~version:version_lexeme () in
      ImportTest.fresh_src_external ~with_proto:true ~proto ~host ~with_path:true ~path ~version ()
    in
    ImportTest.fresh_from ~start:(9, 0, 193) ~src ()
  in
  let aliases =
    let pkg = ImportTest.fresh_pkgpath ~start:(10, 4, 243) ~segs:["first"; "external"; "pkg"; "path"] () in
    let alias =
      let id = Sym.gen seq ~id:external_localname_lexeme in
      ImportTest.fresh_name ~start:(10, 33, 272) ~id ()
    in
    let pkg' = ImportTest.fresh_pkgpath ~start:(11, 4, 294) ~segs:["second"; "external"; "pkg"; "path"] () in
    [
      ImportTest.fresh_alias ~pkg ~with_alias:true ~alias ();
      ImportTest.fresh_alias ~pkg:pkg' ~with_alias:false ()
    ]
  in
  let pkgs = ImportTest.fresh_pkgs ~start:(9, 39, 232) ~aliases () in
  ImportTest.fresh_import ~start:(9, 0, 195) ~stdlib:false ~from ~pkgs ()

let let_bound =
  let binding =
    let patt =
      let id = Sym.gen seq ~id:let_bound_lexeme in
      PattTest.fresh_patt_var ~start:(13, 4, 326) ~id ()
    in
    let ty =
      let id = Sym.gen seq ~id:let_type_lexeme in
      TypeTest.fresh_ty_constr ~start:(13, 17, 339) ~id ()
    in
    let value = ExprTest.fresh_atom_int ~start:(13, 23, 345) ~lexeme:let_value_lexeme () in
    ExprTest.fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  TopTest.fresh_top_let ~start:(13, 0, 322) ~binding ()

let let_bound' =
  let binding =
    let patt =
      let id = Sym.gen seq ~id:let_bound_lexeme' in
      PattTest.fresh_patt_var ~start:(14, 4, 352) ~id ()
    in
    let value = ExprTest.fresh_atom_bool ~start:(14, 18, 366) ~value:true () in
    ExprTest.fresh_value_binding ~patt ~explicit:false ~value ()
  in
  TopTest.fresh_top_let ~start:(14, 0, 348) ~binding ()

let val_bound =
  let binding =
    let patt =
      let id = Sym.gen seq ~id:val_bound_lexeme in
      PattTest.fresh_patt_var ~start:(16, 4, 376) ~id ()
    in
    let ty =
      let id = Sym.gen seq ~id:val_type_lexeme in
      TypeTest.fresh_ty_constr ~start:(16, 17, 389) ~id ()
    in
    let value = ExprTest.fresh_atom_float ~start:(16, 25, 397) ~lexeme:val_value_lexeme () in
    ExprTest.fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  TopTest.fresh_top_val ~start:(16, 0, 372) ~binding ()

let val_bound' =
  let binding =
    let patt =
      let id = Sym.gen seq ~id:val_bound_lexeme' in
      PattTest.fresh_patt_var ~start:(17, 4, 405) ~id ()
    in
    let value =
      let seg = ExprTest.fresh_str_lit ~start:(17, 19, 420) ~value:"foobar" () in
      ExprTest.fresh_atom_string ~start:(17, 18, 419) ~value:[[seg]] ()
    in
    ExprTest.fresh_value_binding ~patt ~explicit:false ~value ()
  in
  TopTest.fresh_top_val ~start:(17, 0, 401) ~binding ()

let imports = [
  stdlib_import;
  current_import;
  external_import;
]

let tops = [
  let_bound;
  let_bound';
  val_bound;
  val_bound';
]

(* Assertions *)

let assert_parses_package_only = ParseUtils.assert_parses_file Parse.parse_package_only SyntaxTest.assert_file_equal
let assert_parses_imports_only = ParseUtils.assert_parses_file Parse.parse_imports_only SyntaxTest.assert_file_equal
let assert_parses_file = ParseUtils.assert_parses_file Parse.parse_file SyntaxTest.assert_file_equal

(* Tests *)

let test_package_only ctxt =
  Syntax.file pkg [] []
    |> assert_parses_package_only ~ctxt source

let test_imports_only ctxt =
  Syntax.file pkg imports []
    |> assert_parses_imports_only ~ctxt source

let test_file ctxt =
  Syntax.file pkg imports tops
    |> assert_parses_file ~ctxt source

(* Test Suite *)

let suite =
  "Source Files" >::: [
    "Package Only" >:: test_package_only;
    "Imports Only" >:: test_imports_only;
    "Whole Files"  >:: test_file;
  ]
