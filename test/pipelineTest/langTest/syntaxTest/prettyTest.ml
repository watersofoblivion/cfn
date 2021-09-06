open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let assert_pp_ty = PrettyTest.assert_pp Syntax.pp_ty
let assert_pp_un = PrettyTest.assert_pp Syntax.pp_un
let assert_pp_bin = PrettyTest.assert_pp Syntax.pp_bin
let assert_pp_rune = PrettyTest.assert_pp Syntax.pp_rune
let assert_pp_str = PrettyTest.assert_pp Syntax.pp_str
let assert_pp_expr = PrettyTest.assert_pp Syntax.pp_expr
let assert_pp_patt = PrettyTest.assert_pp Syntax.pp_patt
let assert_pp_binding = PrettyTest.assert_pp Syntax.pp_binding
let assert_pp_top = PrettyTest.assert_pp Syntax.pp_top
let assert_pp_name = PrettyTest.assert_pp Syntax.pp_name
let assert_pp_src = PrettyTest.assert_pp Syntax.pp_src
let assert_pp_from = PrettyTest.assert_pp Syntax.pp_from
let assert_pp_alias = PrettyTest.assert_pp Syntax.pp_alias
let assert_pp_pkgs = PrettyTest.assert_pp Syntax.pp_pkgs
let assert_pp_import = PrettyTest.assert_pp Syntax.pp_import
let assert_pp_pkg = PrettyTest.assert_pp Syntax.pp_pkg
let assert_pp_file = PrettyTest.assert_pp Syntax.pp_file

(* Pretty Printing *)

let test_ty_constr ctxt =
  let constr =
    ()
      |> Sym.seq
      |> Sym.gen ~id:Prim.id_bool
  in
  Syntax.ty_constr LocTest.dummy constr
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_expr_bool ctxt =
  Syntax.expr_bool LocTest.dummy true
    |> assert_pp_expr ~ctxt ["true"];
  Syntax.expr_bool LocTest.dummy false
    |> assert_pp_expr ~ctxt ["false"]

let test_expr_int ctxt =
  Syntax.expr_int LocTest.dummy "+42i"
    |> assert_pp_expr ~ctxt ["+42i"]

let test_expr_long ctxt =
  Syntax.expr_long LocTest.dummy "+42L"
    |> assert_pp_expr ~ctxt ["+42L"]

let test_expr_float ctxt =
  Syntax.expr_float LocTest.dummy "+1.2e-3.4f"
    |> assert_pp_expr ~ctxt ["+1.2e-3.4f"]

let test_expr_double ctxt =
  Syntax.expr_double LocTest.dummy "+1.2e-3.4D"
    |> assert_pp_expr ~ctxt ["+1.2e-3.4D"]

let test_expr_rune ctxt =
  let value = 'a' in
  AstTest.fresh_expr_rune ~value ()
    |> assert_pp_expr ~ctxt [
         sprintf "'%c'" value
       ]

let test_expr_string ctxt =
  let value = "asdf" in
  AstTest.fresh_expr_string ~value ()
    |> assert_pp_expr ~ctxt [
         sprintf "%S" value
       ]

let test_patt_ground ctxt =
  AstTest.fresh_patt_ground ()
    |> assert_pp_patt ~ctxt ["_"]

let test_patt_var ctxt =
  let id = "testId" in
  AstTest.fresh_patt_var ~id ()
    |> assert_pp_patt ~ctxt [id]

let test_binding_value_binding ctxt =
  let id = "testId" in
  AstTest.fresh_value_binding ~explicit:true ~id ()
    |> assert_pp_binding ~ctxt [
         sprintf "%s: %s = %B" id Prim.id_bool true
       ];
  AstTest.fresh_value_binding ~explicit:false ~id ()
    |> assert_pp_binding ~ctxt [
         sprintf "%s = %B" id true
       ]

let test_top_let ctxt =
  let loc = LocTest.gen () in
  let id = "testId" in
  let binding = AstTest.fresh_value_binding ~id () in
  Syntax.top_let loc binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Syntax.pp_binding binding |> flush_str_formatter
       ]

let test_top_val ctxt =
  let loc = LocTest.gen () in
  let id = "testId" in
  let binding = AstTest.fresh_value_binding ~id () in
  Syntax.top_val loc binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "val %a" Syntax.pp_binding binding |> flush_str_formatter
       ]

let test_name ctxt =
  let id = "testpackage" in
  AstTest.fresh_name ~id ()
    |> assert_pp_name ~ctxt [id]

let test_src ctxt =
  let name = "testsource" in
  AstTest.fresh_src ~name ()
    |> assert_pp_src ~ctxt [name]

let test_from ctxt =
  let src = "testsource" in
  AstTest.fresh_from ~src ()
    |> assert_pp_from ~ctxt [
         sprintf "from %s " src
       ]

let test_alias_no_local_name ctxt =
  let pkg = "testpkg" in
  AstTest.fresh_alias ~pkg ()
    |> assert_pp_alias ~ctxt [pkg]

let test_alias_local_name ctxt =
  let pkg = "testpkg" in
  let local = "testlocal" in
  AstTest.fresh_alias ~pkg ~local ()
    |> assert_pp_alias ~ctxt [
         sprintf "%s -> %s" pkg local
       ]

let test_pkgs ctxt =
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkgs =
    Syntax.pkgs LocTest.dummy [
      AstTest.fresh_alias ~pkg ~local ();
      AstTest.fresh_alias ~pkg:pkg' ()
    ]
  in
  pkgs
    |> assert_pp_pkgs ~ctxt [
         "";
         sprintf "| %s -> %s" pkg local;
         sprintf "| %s" pkg'
       ]

let test_import_from ctxt =
  let src = "testsrc" in
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let from = AstTest.fresh_from ~src () in
  let pkgs =
    Syntax.pkgs LocTest.dummy [
      AstTest.fresh_alias ~pkg ~local ();
      AstTest.fresh_alias ~pkg:pkg' ()
    ]
  in

  Syntax.import LocTest.dummy (Some from) pkgs
    |> assert_pp_import ~ctxt [
         sprintf "from %s import" src;
         sprintf "                   | %s -> %s" pkg local;
         sprintf "                   | %s" pkg'
       ]

let test_import ctxt =
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkgs =
    Syntax.pkgs LocTest.dummy [
      AstTest.fresh_alias ~pkg ~local ();
      AstTest.fresh_alias ~pkg:pkg' ()
    ]
  in

  Syntax.import LocTest.dummy None pkgs
    |> assert_pp_import ~ctxt [
         sprintf "import";
         sprintf "      | %s -> %s" pkg local;
         sprintf "      | %s" pkg'
       ]

let test_pkg ctxt =
  let id = "testpackage" in
  ()
    |> AstTest.fresh_pkg ~id
    |> assert_pp_pkg ~ctxt [
         sprintf "package %s" id
       ]

let test_file_no_imports ctxt =
  let id = "testpackage" in

  let pkg = AstTest.fresh_pkg ~id () in
  Syntax.file pkg [] []
    |> assert_pp_file ~ctxt [
         sprintf "package %s" id
       ]

let test_file_with_imports ctxt =
  let id = "testpackage" in
  let src = "testsrc" in
  let pkg = "testpkg" in
  let local = "testlocal" in
  let pkg' = "anotherpkg" in

  let pkg_stmt = AstTest.fresh_pkg ~id () in
  let imports =
    let pkgs =
      Syntax.pkgs LocTest.dummy [
        AstTest.fresh_alias ~pkg ~local ();
        AstTest.fresh_alias ~pkg:pkg' ()
      ]
    in

    let import =
      let from = AstTest.fresh_from ~src () in
      Syntax.import LocTest.dummy (Some from) pkgs
    in
    let import' = Syntax.import LocTest.dummy None pkgs in
    [import; import']
  in
  Syntax.file pkg_stmt imports []
    |> assert_pp_file ~ctxt [
         sprintf "package %s" id;
         "";
         sprintf "from %s import" src;
         sprintf "                   | %s -> %s" pkg local;
         sprintf "                   | %s" pkg';
         sprintf "import";
         sprintf "      | %s -> %s" pkg local;
         sprintf "      | %s" pkg'
       ]

(* Test Suite *)
let suite =
  "Pretty Printing" >::: [
    "Types" >:: test_ty_constr;
    "Expressions" >::: [
      "Booleans" >:: test_expr_bool;
      "Integers" >:: test_expr_int;
      "Longs"    >:: test_expr_long;
      "Floats"   >:: test_expr_float;
      "Doubles"  >:: test_expr_double;
      "Runes"    >:: test_expr_rune;
      "Strings"  >:: test_expr_string;
    ];
    "Patterns" >::: [
      "Ground"     >:: test_patt_ground;
      "Identifier" >:: test_patt_var;
    ];
    "Bindings" >::: [
      "Value Bindings" >:: test_binding_value_binding;
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
      "Val Bindings" >:: test_top_val;
    ];
    "Names" >:: test_name;
    "Imports" >::: [
      "Sources"     >:: test_src;
      "From Clause" >:: test_from;
      "Alias Clause" >::: [
        "With Local Name"    >:: test_alias_local_name;
        "Without Local Name" >:: test_alias_no_local_name;
      ];
      "Package List" >:: test_pkgs;
      "Import Statement" >::: [
        "From"  >:: test_import_from;
        "Local" >:: test_import;
      ];
    ];
    "Package Statements" >:: test_pkg;
    "Files" >::: [
      "With Imports" >:: test_file_with_imports;
      "No Imports"   >:: test_file_no_imports;
    ];
  ]
