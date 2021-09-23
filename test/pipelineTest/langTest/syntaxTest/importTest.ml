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

let fresh_proto ?loc:(loc = LocTest.gen ()) ?proto:(proto = "https") _ =
  Syntax.proto loc proto

let fresh_host ?loc:(loc = LocTest.gen ()) ?host:(host = "github.com") _ =
  Syntax.host loc host

let fresh_hostpath ?loc:(loc = LocTest.gen ()) ?segs:(segs = ["the"; "path"]) _ =
  Syntax.hostpath loc segs

let fresh_version ?loc:(loc = LocTest.gen ()) ?version:(version = "42") _ =
  Syntax.version loc version

let fresh_src_current ?loc:(loc = LocTest.gen ()) _ =
  Syntax.src_current loc

let fresh_src_external ?loc:(loc = LocTest.gen ()) ?with_proto:(with_proto = false) ?proto:(proto = fresh_proto ()) ?host:(host = fresh_host ()) ?with_path:(with_path = false) ?path:(path = fresh_hostpath ()) ?version:(version = fresh_version ()) _ =
  let proto = if with_proto then Some proto else None in
  let path = if with_path then Some path else None in
  Syntax.src_external loc proto host path version

let fresh_from ?loc:(loc = LocTest.gen ()) ?src:(src = fresh_src_external ()) _ =
  Syntax.from loc src

let fresh_pkgpath ?loc:(loc = LocTest.gen ()) ?path:(path = [ExprTest.fresh_str_lit ()]) _ =
  Syntax.pkgpath loc path

let fresh_alias ?loc:(loc = LocTest.gen ()) ?pkg:(pkg = fresh_pkgpath ()) ?with_alias:(with_alias = false) ?alias:(alias = fresh_name ~id:"alias" ()) _ =
  let alias = if with_alias then Some alias else None in
  Syntax.alias loc pkg alias

let fresh_pkgs ?loc:(loc = LocTest.gen ()) ?aliases:(aliases = []) _ =
  Syntax.pkgs loc aliases

let fresh_import ?loc:(loc = LocTest.gen ()) ?stdlib:(stdlib = true) ?from:(from = fresh_from ()) ?pkgs:(pkgs = fresh_pkgs ()) _ =
  let from = if stdlib then None else Some from in
  Syntax.import loc from pkgs

let fresh_pkg ?loc:(loc = LocTest.gen ()) ?id:(id = fresh_name ()) _ =
  Syntax.pkg loc id

(* Location Stripping *)

let deloc_name = function
  | Syntax.Name name -> Syntax.name LocTest.dummy name.id

let deloc_proto = function
  | Syntax.Proto proto -> Syntax.proto LocTest.dummy proto.proto

let deloc_host = function
  | Syntax.Host host -> Syntax.host LocTest.dummy host.host

let deloc_hostpath = function
  | Syntax.HostPath path -> Syntax.hostpath LocTest.dummy path.segs

let deloc_version = function
  | Syntax.Version version -> Syntax.version LocTest.dummy version.version

let deloc_src = function
  | Syntax.Current _ -> Syntax.src_current LocTest.dummy
  | Syntax.External src ->
    let proto = SyntaxUtils.deloc_optional deloc_proto src.proto in
    let host = deloc_host src.host in
    let path = SyntaxUtils.deloc_optional deloc_hostpath src.path in
    let version = deloc_version src.version in
    Syntax.src_external LocTest.dummy proto host path version

let deloc_from = function
  | Syntax.From from ->
    from.src
      |> deloc_src
      |> Syntax.from LocTest.dummy

let deloc_pkgpath = function
  | Syntax.PkgPath path ->
    path.path
      |> List.map ExprTest.deloc_str
      |> Syntax.pkgpath LocTest.dummy

let deloc_alias = function
  | Syntax.Alias alias ->
    let pkg = deloc_pkgpath alias.pkg in
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

let src_not_equal = TestUtils.not_equal "Import sources" Syntax.pp_src

let assert_name_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Name expected, Syntax.Name actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id

let assert_proto_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Proto expected, Syntax.Proto actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Protocols are not equal" expected.proto actual.proto

let assert_host_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Host expected, Syntax.Host actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Hostnames are not equal" expected.host actual.host

let assert_hostpath_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.HostPath expected, Syntax.HostPath actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_equal ~ctxt ~printer:Fun.id ~msg:"Host segments are not equal") expected.segs actual.segs

let assert_version_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Version expected, Syntax.Version actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Versions are not equal" expected.version actual.version

let assert_src_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Current expected, Syntax.Current actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.External expected, Syntax.External actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    TestUtils.assert_optional_equal "Protocols" assert_proto_equal ~ctxt expected.proto actual.proto;
    assert_host_equal ~ctxt expected.host actual.host;
    TestUtils.assert_optional_equal "Hosts" assert_hostpath_equal ~ctxt expected.path actual.path;
    assert_version_equal ~ctxt expected.version actual.version
  | expected, actual -> src_not_equal ~ctxt expected actual

let assert_from_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.From expected, Syntax.From actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_src_equal ~ctxt expected.src actual.src

let assert_pkgpath_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.PkgPath expected, Syntax.PkgPath actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (ExprTest.assert_str_equal ~ctxt) expected.path actual.path

let assert_alias_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.Alias expected, Syntax.Alias actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_pkgpath_equal ~ctxt expected.pkg actual.pkg;
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
    | Syntax.Name actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id

let test_proto ctxt =
  let loc = LocTest.gen () in
  let proto = "https" in
  match Syntax.proto loc proto with
    | Syntax.Proto actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:Fun.id ~msg:"Protocols are not equal" proto actual.proto

let test_host ctxt =
  let loc = LocTest.gen () in
  let host = "github.com" in
  match Syntax.host loc host with
    | Syntax.Host actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:Fun.id ~msg:"Hostnames are not equal" host actual.host

let test_hostpath ctxt =
  let loc = LocTest.gen () in
  let segs = ["path"; "segments"] in
  match Syntax.hostpath loc segs with
    | Syntax.HostPath actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (assert_equal ~ctxt ~printer:Fun.id ~msg:"Path segements are not equal") segs actual.segs

let test_version ctxt =
  let loc = LocTest.gen () in
  let version = "42" in
  match Syntax.version loc version with
    | Syntax.Version actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:Fun.id ~msg:"Versions are not equal" version actual.version

let test_src_current ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.src_current loc in
  match expected with
    | Syntax.Current actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> src_not_equal ~ctxt expected actual

let test_src_external ctxt =
  let loc = LocTest.gen () in
  let host = fresh_host () in
  let version = fresh_version () in
  let assert_src proto path =
    let expected = Syntax.src_external loc proto host path version in
    match expected with
      | Syntax.External actual ->
        LocTest.assert_loc_equal ~ctxt loc actual.loc;
        TestUtils.assert_optional_equal ~ctxt "Protocols" assert_proto_equal proto actual.proto;
        assert_host_equal ~ctxt host actual.host;
        TestUtils.assert_optional_equal ~ctxt "Paths" assert_hostpath_equal path actual.path;
        assert_version_equal ~ctxt version actual.version
      | actual -> src_not_equal ~ctxt expected actual
  in

  assert_src None None;
  let proto = Some (fresh_proto ()) in
  let path = Some (fresh_hostpath ()) in
  assert_src proto path

let test_from ctxt =
  let loc = LocTest.gen () in
  let src = fresh_src_external () in
  match Syntax.from loc src with
    | Syntax.From from ->
      LocTest.assert_loc_equal ~ctxt loc from.loc;
      assert_src_equal ~ctxt src from.src

let test_pkgpath ctxt =
  let loc = LocTest.gen () in
  let path = [ExprTest.fresh_str_lit ()] in
  match Syntax.pkgpath loc path with
    | Syntax.PkgPath actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (ExprTest.assert_str_equal ~ctxt) path actual.path

let test_alias ctxt =
  let loc = LocTest.gen () in
  let pkg = fresh_pkgpath () in
  let assert_alias local =
    match Syntax.alias loc pkg local with
      | Syntax.Alias alias ->
        LocTest.assert_loc_equal ~ctxt loc alias.loc;
        assert_pkgpath_equal ~ctxt pkg alias.pkg;
        TestUtils.assert_optional_equal ~ctxt "alias" assert_name_equal local alias.alias;
  in
  assert_alias None;
  Some (fresh_name ())
    |> assert_alias

let test_pkgs ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let pkg_one = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"pkg/one" ()] () in
  let pkg_two = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"pkg/two" ()] () in
  let alias = fresh_name ~seq ~id:"localName" () in
  let aliases = [
    fresh_alias ~pkg:pkg_one ();
    fresh_alias ~pkg:pkg_two ~with_alias:true ~alias ()
  ] in
  match Syntax.pkgs loc aliases with
    | Syntax.Packages pkgs ->
      LocTest.assert_loc_equal ~ctxt loc pkgs.loc;
      List.iter2 (assert_alias_equal ~ctxt) aliases pkgs.pkgs

let test_import ctxt =
  let seq = Sym.seq () in
  let loc = LocTest.gen () in
  let assert_import from pkgs =
    match Syntax.import loc from pkgs with
      | Syntax.Import import ->
        LocTest.assert_loc_equal ~ctxt loc import.loc;
        TestUtils.assert_optional_equal ~ctxt "from clause" assert_from_equal from import.from;
        assert_pkgs_equal ~ctxt pkgs import.pkgs;
  in

  let from = None in
  fresh_pkgs ()
    |> assert_import from;

  let from = Some (fresh_from ()) in
  let pkg_one = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"pkg/one" ()] () in
  let pkg_two = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"pkg/two" ()] () in
  let alias = fresh_name ~seq ~id:"localName" () in
  let aliases = [
    fresh_alias ~pkg:pkg_one ();
    fresh_alias ~pkg:pkg_two ~with_alias:true ~alias ()
  ] in
  fresh_pkgs ~aliases ()
    |> assert_import from

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
      "Sources"          >::: [
        "Components" >::: [
          "Protocols"      >:: test_proto;
          "Host Names"     >:: test_host;
          "Paths"          >:: test_hostpath;
          "Major Versions" >:: test_version;
        ];
        "Current Package"  >:: test_src_current;
        "External Package" >:: test_src_external;
      ];
      "From Clauses"     >:: test_from;
      "Package Paths"    >:: test_pkgpath;
      "Alias"            >:: test_alias;
      "Package List"     >:: test_pkgs;
      "Import Statement" >:: test_import;
    ];
    "Package Statement" >:: test_pkg;
  ]

(* Locations *)

let assert_loc_name = SyntaxUtils.assert_loc Syntax.loc_name
let assert_loc_proto = SyntaxUtils.assert_loc Syntax.loc_proto
let assert_loc_host = SyntaxUtils.assert_loc Syntax.loc_host
let assert_loc_hostpath = SyntaxUtils.assert_loc Syntax.loc_hostpath
let assert_loc_version = SyntaxUtils.assert_loc Syntax.loc_version
let assert_loc_src = SyntaxUtils.assert_loc Syntax.loc_src
let assert_loc_from = SyntaxUtils.assert_loc Syntax.loc_from
let assert_loc_pkgpath = SyntaxUtils.assert_loc Syntax.loc_pkgpath
let assert_loc_alias = SyntaxUtils.assert_loc Syntax.loc_alias
let assert_loc_pkgs = SyntaxUtils.assert_loc Syntax.loc_pkgs
let assert_loc_import = SyntaxUtils.assert_loc Syntax.loc_import
let assert_loc_pkg = SyntaxUtils.assert_loc Syntax.loc_pkg

let test_loc_name = assert_loc_name (fun loc -> fresh_name ~loc ())
let test_loc_proto = assert_loc_proto (fun loc -> fresh_proto ~loc ())
let test_loc_host = assert_loc_host (fun loc -> fresh_host ~loc ())
let test_loc_hostpath = assert_loc_hostpath (fun loc -> fresh_hostpath ~loc ())
let test_loc_version = assert_loc_version (fun loc -> fresh_version ~loc ())
let test_loc_src_current = assert_loc_src (fun loc -> fresh_src_current ~loc ())
let test_loc_src_external = assert_loc_src (fun loc -> fresh_src_external ~loc ())
let test_loc_from = assert_loc_from (fun loc -> fresh_from ~loc ())
let test_loc_pkgpath = assert_loc_pkgpath (fun loc -> fresh_pkgpath ~loc ())
let test_loc_alias = assert_loc_alias (fun loc -> fresh_alias ~loc ())
let test_loc_pkgs = assert_loc_pkgs (fun loc -> fresh_pkgs ~loc ())
let test_loc_import = assert_loc_import (fun loc -> fresh_import ~loc ())

let test_loc_pkg = assert_loc_pkg (fun loc -> fresh_pkg ~loc ())

let test_loc =
  "Locations" >::: [
    "Names" >:: test_loc_name;
    "Imports" >::: [
      "Sources" >::: [
        "Components" >::: [
          "Protocols"      >:: test_loc_proto;
          "Host Names"     >:: test_loc_host;
          "Paths"          >:: test_loc_hostpath;
          "Major Versions" >:: test_loc_version;
        ];
        "Current Package"  >:: test_loc_src_current;
        "External Package" >:: test_loc_src_external;
      ];
      "From Clauses"     >:: test_loc_from;
      "Package Paths"    >:: test_loc_pkgpath;
      "Alias"            >:: test_loc_alias;
      "Package List"     >:: test_loc_pkgs;
      "Import Statement" >:: test_loc_import;
    ];
    "Package Statement" >:: test_loc_pkg;
  ]

(* Pretty Printing *)

let assert_pp_name = PrettyTest.assert_pp Syntax.pp_name
let assert_pp_proto = PrettyTest.assert_pp Syntax.pp_proto
let assert_pp_host = PrettyTest.assert_pp Syntax.pp_host
let assert_pp_hostpath = PrettyTest.assert_pp Syntax.pp_hostpath
let assert_pp_version = PrettyTest.assert_pp Syntax.pp_version
let assert_pp_src = PrettyTest.assert_pp Syntax.pp_src
let assert_pp_from = PrettyTest.assert_pp Syntax.pp_from
let assert_pp_pkgpath = PrettyTest.assert_pp Syntax.pp_pkgpath
let assert_pp_alias = PrettyTest.assert_pp Syntax.pp_alias
let assert_pp_pkgs = PrettyTest.assert_pp Syntax.pp_pkgs
let assert_pp_import = PrettyTest.assert_pp Syntax.pp_import
let assert_pp_pkg = PrettyTest.assert_pp Syntax.pp_pkg

let test_pp_name ctxt =
  let id = "testpackage" in
  fresh_name ~id ()
    |> assert_pp_name ~ctxt [id]

let test_pp_proto ctxt =
  let proto = "https" in
  fresh_proto ~proto ()
    |> assert_pp_proto ~ctxt [proto]

let test_pp_host ctxt =
  let host = "github.com" in
  fresh_host ~host ()
    |> assert_pp_host ~ctxt [host]

let test_pp_hostpath ctxt =
  let seg1 = "test" in
  let seg2 = "seg" in
  let seg3 = "ments" in
  let segs = [seg1; seg2; seg3] in
  fresh_hostpath ~segs ()
    |> assert_pp_hostpath ~ctxt [sprintf "%s/%s/%s" seg1 seg2 seg3]

let test_pp_version ctxt =
  let version = "42" in
  fresh_version ~version ()
    |> assert_pp_version ~ctxt [version]

let test_pp_src_current ctxt =
  fresh_src_current ()
    |> assert_pp_src ~ctxt ["."]

let test_pp_src_external ctxt =
  let proto = fresh_proto ~proto:"https" () in
  let host = fresh_host ~host:"github.com" () in
  let path = fresh_hostpath ~segs:["test"; "seg"; "ments"] () in
  let version = fresh_version ~version:"42" () in
  fresh_src_external ~host ~version ()
    |> assert_pp_src ~ctxt ["github.com@v42"];
  fresh_src_external ~with_proto:true ~proto ~host ~with_path:true ~path ~version ()
    |> assert_pp_src ~ctxt ["https://github.com/test/seg/ments@v42"]

let test_pp_from ctxt =
  let src = fresh_src_current () in
  fresh_from ~src ()
    |> assert_pp_from ~ctxt ["from . "]

let test_pp_pkgpath ctxt =
  let path = [ExprTest.fresh_str_lit ~value:"test/pkg" ()] in
  fresh_pkgpath ~path ()
    |> assert_pp_pkgpath ~ctxt ["\"test/pkg\""]

let test_pp_alias_no_local_name ctxt =
  let pkg = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"test/pkg" ()] () in
  fresh_alias ~pkg ()
    |> assert_pp_alias ~ctxt ["\"test/pkg\""]

let test_pp_alias_local_name ctxt =
  let local = "testlocal" in

  let pkg = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"test/pkg" ()] () in
  let alias = fresh_name ~id:local () in

  fresh_alias ~pkg ~with_alias:true ~alias ()
    |> assert_pp_alias ~ctxt [
         sprintf "\"test/pkg\" -> %s" local
       ]

let test_pp_pkgs ctxt =
  let local = "testlocal" in

  let pkg = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"test/pkg" ()] () in
  let alias = fresh_name ~id:local () in
  let pkg' = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"another/pkg" ()] () in

  let pkgs =
    Syntax.pkgs LocTest.dummy [
      fresh_alias ~pkg ~with_alias:true ~alias ();
      fresh_alias ~pkg:pkg' ()
    ]
  in
  pkgs
    |> assert_pp_pkgs ~ctxt [
         "";
         sprintf "| \"test/pkg\" -> %s" local;
         sprintf "| \"another/pkg\""
       ]

let test_pp_import_from ctxt =
  let local = "testlocal" in

  let pkg = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"test/pkg" ()] () in
  let alias = fresh_name ~id:local () in
  let pkg' = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"another/pkg" ()] () in

  let from =
    let src = fresh_src_current () in
    fresh_from ~src ()
  in
  let pkgs =
    Syntax.pkgs LocTest.dummy [
      fresh_alias ~pkg ~with_alias:true ~alias ();
      fresh_alias ~pkg:pkg' ()
    ]
  in

  Syntax.import LocTest.dummy (Some from) pkgs
    |> assert_pp_import ~ctxt [
         sprintf "from . import";
         sprintf "             | \"test/pkg\" -> %s" local;
         sprintf "             | \"another/pkg\""
       ]

let test_pp_import ctxt =
  let local = "testlocal" in

  let pkg = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"test/pkg" ()] () in
  let alias = fresh_name ~id:local () in
  let pkg' = fresh_pkgpath ~path:[ExprTest.fresh_str_lit ~value:"another/pkg" ()] () in

  let pkgs =
    Syntax.pkgs LocTest.dummy [
      fresh_alias ~pkg ~with_alias:true ~alias ();
      fresh_alias ~pkg:pkg' ()
    ]
  in

  Syntax.import LocTest.dummy None pkgs
    |> assert_pp_import ~ctxt [
         sprintf "import";
         sprintf "      | \"test/pkg\" -> %s" local;
         sprintf "      | \"another/pkg\""
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
      "Sources"     >::: [
        "Components" >::: [
          "Protocols"      >:: test_pp_proto;
          "Host Names"     >:: test_pp_host;
          "Paths"          >:: test_pp_hostpath;
          "Major Versions" >:: test_pp_version;
        ];
        "Current Package"  >:: test_pp_src_current;
        "External Package" >:: test_pp_src_external;
      ];
      "From Clause"   >:: test_pp_from;
      "Package Paths" >:: test_pp_pkgpath;
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
