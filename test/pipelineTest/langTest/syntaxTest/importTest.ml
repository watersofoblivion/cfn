(* Imports *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_name ?loc:(loc = LocTest.gen ()) ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  seq
    |> Sym.gen ~id
    |> Syntax.name loc

let fresh_src ?loc:(loc = LocTest.gen ()) ?name:(name = fresh_name ()) _ =
  Syntax.src loc name

let fresh_from ?loc:(loc = LocTest.gen ()) ?src:(src = fresh_src ()) _ =
  Syntax.from loc src

let fresh_alias ?loc:(loc = LocTest.gen ()) ?pkg:(pkg = fresh_name ~id:"pkg" ()) ?alias:(alias = false) ?local:(local = fresh_name ~id:"alias" ()) _ =
  let local = if alias then Some local else None in
  Syntax.alias loc pkg local

let fresh_pkgs ?loc:(loc = LocTest.gen ()) ?aliases:(aliases = []) _ =
  Syntax.pkgs loc aliases

let fresh_import ?loc:(loc = LocTest.gen ()) ?stdlib:(stdlib = true) ?from:(from = fresh_from ()) ?pkgs:(pkgs = fresh_pkgs ()) _ =
  let from = if stdlib then Some from else None in
  Syntax.import loc from pkgs

let fresh_pkg ?loc:(loc = LocTest.gen ()) ?id:(id = fresh_name ()) _ =
  Syntax.pkg loc id

(* Location Stripping *)

let deloc_name = function
  | Syntax.Name name -> Syntax.name LocTest.dummy name.id

let deloc_src = function
  | Syntax.Source source ->
    source.name
      |> deloc_name
      |> Syntax.src LocTest.dummy

let deloc_from = function
  | Syntax.From from ->
    from.src
      |> deloc_src
      |> Syntax.from LocTest.dummy

let deloc_alias = function
  | Syntax.Alias alias ->
    let pkg = deloc_name alias.pkg in
    alias.alias
      |> SyntaxUtils.deloc_optional deloc_name
      |> Syntax.alias LocTest.dummy pkg

let deloc_pkgs = function
  | Syntax.Packages pkgs ->
    pkgs.pkgs
      |> List.map deloc_alias
      |> Syntax.pkgs LocTest.dummy

let deloc_import = function
  | Syntax.Import import ->
    let from = SyntaxUtils.deloc_optional deloc_from import.from in
    import.pkgs
      |> deloc_pkgs
      |> Syntax.import LocTest.dummy from

let deloc_pkg = function
  | Syntax.Package pkg ->
    pkg.id
      |> deloc_name
      |> Syntax.pkg LocTest.dummy

(* Assertions *)

let assert_name_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Name expected, Syntax.Name actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id

let assert_src_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Source expected, Syntax.Source actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.name actual.name

let assert_from_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.From expected, Syntax.From actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_src_equal ~ctxt expected.src actual.src

let assert_alias_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Alias expected, Syntax.Alias actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.pkg actual.pkg;
    TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal expected.alias actual.alias

let assert_pkgs_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Packages expected, Syntax.Packages actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_alias_equal ~ctxt) expected.pkgs actual.pkgs

let assert_import_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Import expected, Syntax.Import actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal expected.from actual.from;
    assert_pkgs_equal ~ctxt expected.pkgs actual.pkgs

let assert_pkg_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Package expected, Syntax.Package actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_name_equal ~ctxt expected.id actual.id

(* Tests *)

(* Constructors *)

let test_name ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  match Syntax.name loc id with
    | Syntax.Name name ->
      LocTest.assert_loc_equal ~ctxt loc name.loc;
      SymTest.assert_sym_equal ~ctxt id name.id

let test_src ctxt =
  let loc = LocTest.gen () in
  let name = fresh_name () in
  match Syntax.src loc name with
    | Syntax.Source src ->
      LocTest.assert_loc_equal ~ctxt loc src.loc;
      assert_name_equal ~ctxt name src.name

let test_from ctxt =
  let loc = LocTest.gen () in
  let src = fresh_src () in
  match Syntax.from loc src with
    | Syntax.From from ->
      LocTest.assert_loc_equal ~ctxt loc from.loc;
      assert_src_equal ~ctxt src from.src

let test_alias ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let pkg = fresh_name ~seq () in

  let local = None in
  match Syntax.alias loc pkg local with
    | Syntax.Alias alias ->
      LocTest.assert_loc_equal ~ctxt loc alias.loc;
      assert_name_equal ~ctxt pkg alias.pkg;
      TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal local alias.alias;

  let local = Some (fresh_name ~seq ()) in
  match Syntax.alias loc pkg local with
    | Syntax.Alias alias ->
      LocTest.assert_loc_equal ~ctxt loc alias.loc;
      assert_name_equal ~ctxt pkg alias.pkg;
      TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal local alias.alias

let test_pkgs ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let pkg_one = fresh_name ~seq ~id:"pkgOne" () in
  let pkg_two = fresh_name ~seq ~id:"pkgTwo" () in
  let local = fresh_name ~seq ~id:"localName" () in
  let aliases = [
    fresh_alias ~pkg:pkg_one ();
    fresh_alias ~pkg:pkg_two ~alias:true ~local ()
  ] in
  match Syntax.pkgs loc aliases with
    | Syntax.Packages pkgs ->
      LocTest.assert_loc_equal ~ctxt loc pkgs.loc;
      List.iter2 (assert_alias_equal ~ctxt) aliases pkgs.pkgs

let test_import ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in

  let from = None in
  let pkgs = fresh_pkgs () in
  match Syntax.import loc from pkgs with
    | Syntax.Import import ->
      LocTest.assert_loc_equal ~ctxt loc import.loc;
      TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
      assert_pkgs_equal ~ctxt pkgs import.pkgs;

  let from = Some (fresh_from ()) in
  let pkgs =
    let pkg_one = fresh_name ~seq ~id:"pkgOne" () in
    let pkg_two = fresh_name ~seq ~id:"pkgTwo" () in
    let local = fresh_name ~seq ~id:"localName" () in
    let aliases = [
      fresh_alias ~pkg:pkg_one ();
      fresh_alias ~pkg:pkg_two ~alias:true ~local ()
    ] in
    fresh_pkgs ~aliases ()
  in
  match Syntax.import loc from pkgs with
    | Syntax.Import import ->
      LocTest.assert_loc_equal ~ctxt loc import.loc;
      TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
      assert_pkgs_equal ~ctxt pkgs import.pkgs

let test_pkg ctxt =
  let loc = LocTest.gen () in
  let id = fresh_name () in
  match Syntax.pkg loc id with
    | Syntax.Package pkg ->
      LocTest.assert_loc_equal ~ctxt loc pkg.loc;
      assert_name_equal ~ctxt id pkg.id

let test_constructor =
  "Constructors" >::: [
    "Names" >:: test_name;
    "Imports" >::: [
      "Sources"          >:: test_src;
      "From Clauses"     >:: test_from;
      "Alias"            >:: test_alias;
      "Package List"     >:: test_pkgs;
      "Import Statement" >:: test_import;
    ];
    "Package Statement" >:: test_pkg;
  ]

(* Locations *)

let assert_loc_name = SyntaxUtils.assert_loc Syntax.loc_name
let assert_loc_src = SyntaxUtils.assert_loc Syntax.loc_src
let assert_loc_from = SyntaxUtils.assert_loc Syntax.loc_from
let assert_loc_alias = SyntaxUtils.assert_loc Syntax.loc_alias
let assert_loc_pkgs = SyntaxUtils.assert_loc Syntax.loc_pkgs
let assert_loc_import = SyntaxUtils.assert_loc Syntax.loc_import
let assert_loc_pkg = SyntaxUtils.assert_loc Syntax.loc_pkg

let test_loc_name = assert_loc_name (fun loc -> fresh_name ~loc ())
let test_loc_src = assert_loc_src (fun loc -> fresh_src ~loc ())
let test_loc_from = assert_loc_from (fun loc -> fresh_from ~loc ())
let test_loc_alias = assert_loc_alias (fun loc -> fresh_alias ~loc ())
let test_loc_pkgs = assert_loc_pkgs (fun loc -> fresh_pkgs ~loc ())
let test_loc_import = assert_loc_import (fun loc -> fresh_import ~loc ())

let test_loc_pkg = assert_loc_pkg (fun loc -> fresh_pkg ~loc ())

let test_loc =
  "Locations" >::: [
    "Names" >:: test_loc_name;
    "Imports" >::: [
      "Sources"          >:: test_loc_src;
      "From Clauses"     >:: test_loc_from;
      "Alias"            >:: test_loc_alias;
      "Package List"     >:: test_loc_pkgs;
      "Import Statement" >:: test_loc_import;
    ];
    "Package Statement" >:: test_loc_pkg;
  ]

(* Pretty Printing *)

let assert_pp_name = PrettyTest.assert_pp Syntax.pp_name
let assert_pp_src = PrettyTest.assert_pp Syntax.pp_src
let assert_pp_from = PrettyTest.assert_pp Syntax.pp_from
let assert_pp_alias = PrettyTest.assert_pp Syntax.pp_alias
let assert_pp_pkgs = PrettyTest.assert_pp Syntax.pp_pkgs
let assert_pp_import = PrettyTest.assert_pp Syntax.pp_import
let assert_pp_pkg = PrettyTest.assert_pp Syntax.pp_pkg

let test_pp_name ctxt =
  let id = "testpackage" in
  fresh_name ~id ()
    |> assert_pp_name ~ctxt [id]

let test_pp_src ctxt =
  let id = "testsource" in
  let name = fresh_name ~id () in
  fresh_src ~name ()
    |> assert_pp_src ~ctxt [id]

let test_pp_from ctxt =
  let id = "testsource" in
  let src =
    let name = fresh_name ~id () in
    fresh_src ~name ()
  in
  fresh_from ~src ()
    |> assert_pp_from ~ctxt [
         sprintf "from %s " id
       ]

let test_pp_alias_no_local_name ctxt =
  let id = "testpkg" in
  let pkg = fresh_name ~id () in
  fresh_alias ~pkg ()
    |> assert_pp_alias ~ctxt [id]

let test_pp_alias_local_name ctxt =
  let id = "testpkg" in
  let alias = "testlocal" in

  let pkg = fresh_name ~id () in
  let local = fresh_name ~id:alias () in

  fresh_alias ~pkg ~alias:true ~local ()
    |> assert_pp_alias ~ctxt [
         sprintf "%s -> %s" id alias
       ]

let test_pp_pkgs ctxt =
  let id = "testpkg" in
  let alias = "testlocal" in
  let id' = "anotherpkg" in

  let pkg = fresh_name ~id () in
  let local = fresh_name ~id:alias () in
  let pkg' = fresh_name ~id:id' () in

  let pkgs =
    Syntax.pkgs LocTest.dummy [
      fresh_alias ~pkg ~alias:true ~local ();
      fresh_alias ~pkg:pkg' ()
    ]
  in
  pkgs
    |> assert_pp_pkgs ~ctxt [
         "";
         sprintf "| %s -> %s" id alias;
         sprintf "| %s" id'
       ]

let test_pp_import_from ctxt =
  let src = "testsrc" in
  let id = "testpkg" in
  let alias = "testlocal" in
  let id' = "anotherpkg" in

  let pkg = fresh_name ~id () in
  let local = fresh_name ~id:alias () in
  let pkg' = fresh_name ~id:id' () in

  let from =
    let src =
      let name = fresh_name ~id:src () in
      fresh_src ~name ()
    in
    fresh_from ~src ()
  in
  let pkgs =
    Syntax.pkgs LocTest.dummy [
      fresh_alias ~pkg ~local ();
      fresh_alias ~pkg:pkg' ()
    ]
  in

  Syntax.import LocTest.dummy (Some from) pkgs
    |> assert_pp_import ~ctxt [
         sprintf "from %s import" src;
         sprintf "                   | %s -> %s" id alias;
         sprintf "                   | %s" id'
       ]

let test_pp_import ctxt =
  let id = "testpkg" in
  let alias = "testlocal" in
  let id' = "anotherpkg" in

  let pkg = fresh_name ~id () in
  let local = fresh_name ~id:alias () in
  let pkg' = fresh_name ~id:id' () in

  let pkgs =
    Syntax.pkgs LocTest.dummy [
      fresh_alias ~pkg ~local ();
      fresh_alias ~pkg:pkg' ()
    ]
  in

  Syntax.import LocTest.dummy None pkgs
    |> assert_pp_import ~ctxt [
         sprintf "import";
         sprintf "      | %s -> %s" id alias;
         sprintf "      | %s" id'
       ]

let test_pp_pkg ctxt =
  let id = "testpackage" in
  let name = fresh_name ~id () in
  fresh_pkg ~id:name ()
    |> assert_pp_pkg ~ctxt [
         sprintf "package %s" id
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Names" >:: test_pp_name;
    "Imports" >::: [
      "Sources"     >:: test_pp_src;
      "From Clause" >:: test_pp_from;
      "Alias Clause" >::: [
        "With Local Name"    >:: test_pp_alias_local_name;
        "Without Local Name" >:: test_pp_alias_no_local_name;
      ];
      "Package List" >:: test_pp_pkgs;
      "Import Statement" >::: [
        "From"  >:: test_pp_import_from;
        "Local" >:: test_pp_import;
      ];
    ];
    "Package Statements" >:: test_pp_pkg;
  ]

(* Test Suite *)

let suite =
  "Imports" >::: [
    test_constructor;
    test_loc;
    test_pp;
  ]
