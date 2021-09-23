(* Imports *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_name ?start:(start = ParseUtils.bof) ?id:(id = SymTest.fresh_sym ()) _ =
  let loc = ParseUtils.sym_loc start id in
  Syntax.name loc id

let fresh_proto ?start:(start = ParseUtils.bof) ?proto:(proto = "https") _ =
  let loc =
    proto
      |> ParseUtils.lexeme_loc start
      |> LocTest.shift (0, 3, 3)
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_proto ~loc ~proto ()

let fresh_host ?start:(start = ParseUtils.bof) ?host:(host = "github.com") _ =
  let loc = ParseUtils.lexeme_loc start host in
  SyntaxTest.fresh_host ~loc ~host ()

let fresh_hostpath ?start:(start = ParseUtils.bof) ?segs:(segs = ["test"; "path"]) _ =
  let loc =
    segs
      |> String.concat "/"
      |> ParseUtils.lexeme_loc start
      |> LocTest.shift (0, 1, 1)
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_hostpath ~loc ~segs ()

let fresh_version ?start:(start = ParseUtils.bof) ?version:(version = "42") _ =
  let loc =
    version
      |> ParseUtils.lexeme_loc start
      |> LocTest.shift (0, 2, 2)
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_version ~loc ~version ()

let fresh_src_current ?start:(start = ParseUtils.bof) _ =
  let loc =
    start
      |> LocTest.make start
      |> LocTest.shift (0, 1, 1)
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_src_current ~loc ()

let fresh_src_external ?with_proto:(with_proto = false) ?proto:(proto = fresh_proto ()) ?host:(host = fresh_host ()) ?with_path:(with_path = false) ?path:(path = fresh_hostpath ()) ?version:(version = fresh_version ()) _ =
  let loc =
    let start = if with_proto then Syntax.loc_proto proto else Syntax.loc_host host in
    version
      |> Syntax.loc_version
      |> LocTest.span start
  in
  SyntaxTest.fresh_src_external ~loc ~with_proto ~proto ~host ~with_path ~path ~version ()

let fresh_from ?start:(start = ParseUtils.bof) ?src:(src = fresh_src_external ()) _ =
  let loc =
    src
      |> Syntax.loc_src
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_from ~loc ~src ()

let fresh_pkgpath ?start:(start = ParseUtils.bof) ?segs:(segs = ["test"; "path"]) _ =
  let value = String.concat "/" segs in
  let loc =
    value
      |> ParseUtils.lexeme_loc start
      |> LocTest.shift (0, 2, 2)
      |> LocTest.span_from start
  in
  let path =
    let (line, col, off) = start in
    [ExprTest.fresh_str_lit ~start:(line, col + 1, off + 1) ~value ()]
  in
  SyntaxTest.fresh_pkgpath ~loc ~path ()

let fresh_alias ?pkg:(pkg = fresh_pkgpath ()) ?with_alias:(with_alias = false) ?alias:(alias = fresh_name ~id:(SymTest.fresh_sym ~id:"testId" ()) ()) _ =
  let loc =
    let start = Syntax.loc_pkgpath pkg in
    if with_alias
    then
      alias
        |> Syntax.loc_name
        |> LocTest.span start
    else start
  in
  SyntaxTest.fresh_alias ~loc ~pkg ~with_alias ~alias ()

let fresh_pkgs ?start:(start = ParseUtils.bof) ?aliases:(aliases = []) _ =
  let loc =
    try
      aliases
        |> List.rev
        |> List.hd
        |> Syntax.loc_alias
        |> LocTest.span_from start
    with Failure _ -> LocTest.make start start
  in
  SyntaxTest.fresh_pkgs ~loc ~aliases ()

let fresh_import ?start:(start = ParseUtils.bof) ?stdlib:(stdlib = true) ?from:(from = fresh_from ()) ?pkgs:(pkgs = fresh_pkgs ()) _ =
  let loc =
    let start = if stdlib then LocTest.make start start else Syntax.loc_from from in
    pkgs
      |> Syntax.loc_pkgs
      |> LocTest.span start
  in
  SyntaxTest.fresh_import ~loc ~stdlib ~from ~pkgs ()

let fresh_pkg ?start:(start = ParseUtils.bof) ?name:(name = fresh_name ()) _ =
  let loc =
    name
      |> Syntax.loc_name
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_pkg ~loc ~id:name ()

(* Assertions *)

let assert_parses_name = ParseUtils.assert_parses Parse.parse_name SyntaxTest.assert_name_equal
let assert_parses_proto = ParseUtils.assert_parses Parse.parse_proto SyntaxTest.assert_proto_equal
let assert_parses_host = ParseUtils.assert_parses Parse.parse_host SyntaxTest.assert_host_equal
let assert_parses_hostpath = ParseUtils.assert_parses Parse.parse_hostpath SyntaxTest.assert_hostpath_equal
let assert_parses_version = ParseUtils.assert_parses Parse.parse_version SyntaxTest.assert_version_equal
let assert_parses_src = ParseUtils.assert_parses Parse.parse_src SyntaxTest.assert_src_equal
let assert_parses_from = ParseUtils.assert_parses Parse.parse_from SyntaxTest.assert_from_equal
let assert_parses_alias = ParseUtils.assert_parses Parse.parse_alias SyntaxTest.assert_alias_equal
let assert_parses_pkgpath = ParseUtils.assert_parses Parse.parse_pkgpath SyntaxTest.assert_pkgpath_equal
let assert_parses_local = ParseUtils.assert_parses Parse.parse_local SyntaxTest.assert_name_equal
let assert_parses_pkgs = ParseUtils.assert_parses Parse.parse_pkgs SyntaxTest.assert_pkgs_equal
let assert_parses_import = ParseUtils.assert_parses Parse.parse_import SyntaxTest.assert_import_equal
let assert_parses_pkg = ParseUtils.assert_parses Parse.parse_pkg SyntaxTest.assert_pkg_equal

(* Tests *)

let test_parse_name ctxt =
  let lexeme = "testname" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_name ~id ()
    |> assert_parses_name ~ctxt [lexeme]

let test_parse_proto ctxt =
  let lexeme = "https" in
  fresh_proto ~proto:lexeme ()
    |> assert_parses_proto ~ctxt [sprintf "%s://" lexeme]

let test_parse_host ctxt =
  let lexeme = "github.com" in
  fresh_host ~host:lexeme ()
    |> assert_parses_host ~ctxt [lexeme]

let test_parse_hostpath ctxt =
  let seg1 = "test" in
  let seg2 = "seg" in
  let seg3 = "ments" in
  let segs = [seg1; seg2; seg3] in
  fresh_hostpath ~segs ()
    |> assert_parses_hostpath ~ctxt [sprintf "/%s/%s/%s" seg1 seg2 seg3]

let test_parse_version ctxt =
  let lexeme = "42" in
  fresh_version ~version:lexeme ()
    |> assert_parses_version ~ctxt [sprintf "@v%s" lexeme]

let test_parse_src_current ctxt =
  fresh_src_current ()
    |> assert_parses_src ~ctxt ["."]

let test_parse_src_external ctxt =
  let proto_lexeme = "https" in
  let host_lexeme = "github.com" in
  let segs = ["owner"; "repo"] in
  let segs_lexeme = "owner/repo" in
  let version_lexeme = "42" in

  let proto_len = String.length proto_lexeme in
  let host_len = String.length host_lexeme in
  let segs_len = String.length segs_lexeme in

  let host = fresh_host ~host:host_lexeme () in
  let version_start = host_len in
  let version = fresh_version ~start:(1, version_start, version_start) ~version:version_lexeme () in
  fresh_src_external ~host ~version ()
    |> assert_parses_src ~ctxt [sprintf "%s@v%s" host_lexeme version_lexeme];

  let proto = fresh_proto ~proto:proto_lexeme () in
  let host_start = proto_len + 3 in
  let host = fresh_host ~start:(1, host_start, host_start) ~host:host_lexeme () in
  let version_start = host_start + host_len in
  let version = fresh_version ~start:(1, version_start, version_start) ~version:version_lexeme () in
  fresh_src_external ~with_proto:true ~proto ~host ~version ()
    |> assert_parses_src ~ctxt [sprintf "%s://%s@v%s" proto_lexeme host_lexeme version_lexeme];

  let host = fresh_host ~host:host_lexeme () in
  let segs_start = host_len in
  let path = fresh_hostpath ~start:(1, host_len, host_len) ~segs () in
  let version_start = segs_start + segs_len + 1 in
  let version = fresh_version ~start:(1, version_start, version_start) ~version:version_lexeme () in
  fresh_src_external ~host ~with_path:true ~path ~version ()
    |> assert_parses_src ~ctxt [sprintf "%s/%s@v%s" host_lexeme segs_lexeme version_lexeme];

  let proto = fresh_proto ~proto:proto_lexeme () in
  let host_start = proto_len + 3 in
  let host = fresh_host ~start:(1, host_start, host_start) ~host:host_lexeme () in
  let segs_start = host_start + host_len in
  let path = fresh_hostpath ~start:(1, segs_start, segs_start) ~segs () in
  let version_start = segs_start + segs_len + 1 in
  let version = fresh_version ~start:(1, version_start, version_start) ~version:version_lexeme () in
  fresh_src_external ~with_proto:true ~proto ~host ~with_path:true ~path ~version ()
    |> assert_parses_src ~ctxt [sprintf "%s://%s/%s@v%s" proto_lexeme host_lexeme segs_lexeme version_lexeme]

let test_parse_pkgpath ctxt =
  let lexeme = "test/path" in
  fresh_pkgpath ~segs:["test"; "path"] ()
    |> assert_parses_pkgpath ~ctxt [sprintf "%S" lexeme]

let test_parse_local ctxt =
  let lexeme = "testname" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_name ~start:(1, 3, 3) ~id ()
    |> assert_parses_local ~ctxt [sprintf "-> %s" lexeme]

let test_parse_alias ctxt =
  let pkgname = "test/pkg" in
  let pkg = fresh_pkgpath ~segs:["test"; "pkg"] () in
  fresh_alias ~pkg ~with_alias:false ()
    |> assert_parses_alias ~ctxt [sprintf "%S" pkgname]

let test_parse_alias_local_name ctxt =
  let pkgname = "test/pkg" in
  let local = "localname" in
  let pkg = fresh_pkgpath ~segs:["test"; "pkg"] () in
  let alias =
    let start = String.length pkgname + 2 + 4 in
    let id = () |> Sym.seq |> Sym.gen ~id:local in
    fresh_name ~start:(1, start, start) ~id ()
  in
  fresh_alias ~pkg ~with_alias:true ~alias ()
    |> assert_parses_alias ~ctxt [sprintf "%S -> %s" pkgname local]

let test_parse_pkgs ctxt =
  let pkgname = "test/pkg" in
  let localname = "localname" in
  let pkgname' = "another/pkg" in

  let kwd_len = 7 in
  let pkg_len = String.length pkgname in
  let local_len = String.length localname in

  let pkg = fresh_pkgpath ~start:(2, 4, kwd_len + 4) ~segs:["test"; "pkg"] () in
  let local_start = 4 + pkg_len + 2 + 4 in
  let alias =
    let id = () |> Sym.seq |> Sym.gen ~id:localname in
    fresh_name ~start:(2, local_start, kwd_len + local_start) ~id ()
  in
  let pkg' = fresh_pkgpath ~start:(3, 4, kwd_len + local_start + local_len + 1 + 4) ~segs:["another"; "pkg"] () in
  let aliases = [
    fresh_alias ~pkg ~with_alias:true ~alias ();
    fresh_alias ~pkg:pkg' ~with_alias:false ()
  ] in
  fresh_pkgs ~aliases ()
    |> assert_parses_pkgs ~ctxt [
                  "import";
          sprintf "  | %S -> %s" pkgname localname;
          sprintf "  | %S" pkgname';
       ];

  let pkg = fresh_pkgpath ~start:(1, kwd_len, kwd_len) ~segs:["test"; "pkg"] () in
  let alias_start = kwd_len + pkg_len + 2 + 4 in
  let alias =
    let id = () |> Sym.seq |> Sym.gen ~id:localname in
    fresh_name ~start:(1, alias_start, alias_start) ~id ()
  in
  let pkg' =
    let start = alias_start + local_len + 3 in
    fresh_pkgpath ~start:(1, start, start) ~segs:["another"; "pkg"] ()
  in
  let aliases = [
    fresh_alias ~pkg ~with_alias:true ~alias ();
    fresh_alias ~pkg:pkg' ~with_alias:false ()
  ] in
  fresh_pkgs ~aliases ()
    |> assert_parses_pkgs ~ctxt [sprintf "import %S -> %s | %S" pkgname localname pkgname']

let test_parse_from ctxt =
  let src = fresh_src_current ~start:(1, 5, 5) () in
  fresh_from ~src ()
    |> assert_parses_from ~ctxt ["from . "];

  let src =
    let proto = fresh_proto ~start:(1, 5, 5) ~proto:"https" () in
    let host = fresh_host ~start:(1, 13, 13) ~host:"github.com" () in
    let path = fresh_hostpath ~start:(1, 23, 23) ~segs:["owner"; "repo"] () in
    let version = fresh_version ~start:(1, 34, 34) ~version:"42" () in
    fresh_src_external ~with_proto:true ~proto ~host ~with_path:true ~path ~version ()
  in
  fresh_from ~src ()
    |> assert_parses_from ~ctxt ["from https://github.com/owner/repo@v42 "]

let test_parse_import ctxt =
  let pkgname = "test/pkg" in
  let localname = "localname" in
  let pkgname' = "another/pkg" in

  let kwd_len = 7 in
  let pkg_len = String.length pkgname in
  let local_len = String.length localname in

  let pkgs =
    let pkg = fresh_pkgpath ~start:(2, 4, kwd_len + 4) ~segs:["test"; "pkg"] () in
    let local_start = 4 + pkg_len + 2 + 4 in
    let alias =
      let id = () |> Sym.seq |> Sym.gen ~id:localname in
      fresh_name ~start:(2, local_start, kwd_len + local_start) ~id ()
    in
    let pkg' = fresh_pkgpath ~start:(3, 4, kwd_len + local_start + local_len + 1 + 4) ~segs:["another"; "pkg"] () in
    let aliases = [
      fresh_alias ~pkg ~with_alias:true ~alias ();
      fresh_alias ~pkg:pkg' ~with_alias:false ()
    ] in
    fresh_pkgs ~aliases ()
  in
  fresh_import ~stdlib:true ~pkgs ()
    |> assert_parses_import ~ctxt [
                  "import";
          sprintf "  | %S -> %s" pkgname localname;
          sprintf "  | %S" pkgname';
       ];

  let from =
    let src =
      let proto = fresh_proto ~start:(1, 5, 5) ~proto:"https" () in
      let host = fresh_host ~start:(1, 13, 13) ~host:"github.com" () in
      let path = fresh_hostpath ~start:(1, 23, 23) ~segs:["owner"; "repo"] () in
      let version = fresh_version ~start:(1, 34, 34) ~version:"42" () in
      fresh_src_external ~with_proto:true ~proto ~host ~with_path:true ~path ~version ()
    in
    fresh_from ~src ()
  in
  let from_len = 39 in

  let pkgs =
    let pkg = fresh_pkgpath ~start:(2, 4, from_len + kwd_len + 4) ~segs:["test"; "pkg"] () in
    let local_start = 4 + pkg_len + 2 + 4 in
    let alias =
      let id = () |> Sym.seq |> Sym.gen ~id:localname in
      fresh_name ~start:(2, local_start, from_len + kwd_len + local_start) ~id ()
    in
    let pkg' = fresh_pkgpath ~start:(3, 4, from_len + kwd_len + local_start + local_len + 1 + 4) ~segs:["another"; "pkg"] () in
    let aliases = [
      fresh_alias ~pkg ~with_alias:true ~alias ();
      fresh_alias ~pkg:pkg' ~with_alias:false ()
    ] in
    fresh_pkgs ~start:(1, from_len, from_len) ~aliases ()
  in
  fresh_import ~stdlib:false ~from ~pkgs ()
    |> assert_parses_import ~ctxt [
                  "from https://github.com/owner/repo@v42 import";
          sprintf "  | %S -> %s" pkgname localname;
          sprintf "  | %S" pkgname';
       ]

let test_parse_pkg ctxt =
  let lexeme = "testname" in
  let name =
    let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
    fresh_name ~start:(1, 8, 8) ~id ()
  in
  fresh_pkg ~name ()
    |> assert_parses_pkg ~ctxt [sprintf "package %s" lexeme]

(* Test Suite *)

let suite =
  "Imports" >::: [
    "Names" >:: test_parse_name;
    "Imports" >::: [
      "Sources" >::: [
        "Components" >::: [
          "Protocols"      >:: test_parse_proto;
          "Host Names"     >:: test_parse_host;
          "Paths"          >:: test_parse_hostpath;
          "Major Versions" >:: test_parse_version;
        ];
        "Current Packages"  >:: test_parse_src_current;
        "External Packages" >:: test_parse_src_external;
      ];
      "Local Name"    >:: test_parse_local;
      "Package Paths" >:: test_parse_pkgpath;
      "Alias Clause" >::: [
        "No Local Name"   >:: test_parse_alias;
        "With Local Name" >:: test_parse_alias_local_name;
      ];
      "Alias Lists"    >:: test_parse_pkgs;
      "From Clauses"   >:: test_parse_from;
      "Import Clauses" >:: test_parse_import;
    ];
    "Package Statements" >:: test_parse_pkg;
  ]
