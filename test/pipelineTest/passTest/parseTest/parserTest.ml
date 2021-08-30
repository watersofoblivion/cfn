open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let lexeme_loc_of_len (start_line, start_col, start_byte) len =
  LocTest.make (start_line, start_col, start_byte) (start_line, start_col + len, start_byte + len)

let lexeme_loc start lexeme =
  lexeme
    |> String.length
    |> lexeme_loc_of_len start

let fresh_of_lexeme ?start:(start = (0, 0, 0)) to_lexeme of_lexeme value =
  let lexeme = to_lexeme value in
  let loc = lexeme_loc start lexeme in
  of_lexeme loc lexeme

let fresh_ty_constr ?start:(start = (0, 0, 0)) ?seq:(seq = Sym.seq ()) ?id:(id = "FooBar") _ =
  let loc =
    id
      |> Utf8.len
      |> lexeme_loc_of_len start
  in
  seq
    |> Sym.gen ~id
    |> Syntax.ty_constr loc

let fresh_bool ?start:(start = (0, 0, 0)) ?value:(value = true) _ =
  let loc =
    (if value then 4 else 5)
      |> lexeme_loc_of_len start
  in
  Syntax.expr_bool loc value

let fresh_int ?start:(start = (0, 0, 0)) ?value:(value = 42l) _ =
  fresh_of_lexeme ~start Int32.to_string Syntax.expr_int value

let fresh_long ?start:(start = (0, 0, 0)) ?value:(value = 42L) _ =
  fresh_of_lexeme ~start Int64.to_string Syntax.expr_long value

let fresh_float ?start:(start = (0, 0, 0)) ?value:(value = 4.2) _ =
  fresh_of_lexeme ~start (sprintf "%g") Syntax.expr_float value

let fresh_double ?start:(start = (0, 0, 0)) ?value:(value = 4.2) _ =
  fresh_of_lexeme ~start (sprintf "%g") Syntax.expr_double value

let fresh_rune ?start:(start = (0, 0, 0)) ?value:(value = 'a') _ =
  let lexeme = Uchar.of_char value in
  let loc =
    let (start_line, start_col, start_byte) = start in
    LocTest.make start (start_line, start_col + 3, start_byte + 3)
  in
  Syntax.expr_rune loc lexeme

let fresh_string ?start:(start = (0, 0, 0)) ?value:(value = "foo bar") _ =
  let lexeme = Utf8.normalize value in
  let loc =
    let (start_line, start_col, start_byte) = start in
    let len = Utf8.len value in
    LocTest.make start (start_line, start_col + len, start_byte + len)
  in
  Syntax.expr_string loc lexeme

let fresh_ident ?start:(start = (0, 0, 0)) ?seq:(seq = Sym.seq ()) ?id:(id = "fooBar") _ =
  let loc =
    id
      |> Utf8.len
      |> lexeme_loc_of_len start
  in
  seq
    |> Sym.gen ~id
    |> Syntax.expr_ident loc

let fresh_patt_ground ?start:((start_line, start_col, start_byte) = (0, 0, 0)) _ =
  LocTest.make (start_line, start_col, start_byte) (start_line, start_col + 1, start_byte + 1)
    |> Syntax.patt_ground

let fresh_patt_var ?start:(start = (0, 0, 0)) ?seq:(seq = Sym.seq ()) ?id:(id = "fooBar") _ =
  let loc =
    id
      |> Utf8.len
      |> lexeme_loc_of_len start
  in
  seq
    |> Sym.gen ~id
    |> Syntax.patt_var loc

let fresh_name ?start:(start = (0, 0, 0)) ?seq:(seq = Sym.seq ()) ?id:(id = "fooBar") _ =
  let loc =
    id
      |> Utf8.len
      |> lexeme_loc_of_len start
  in
  seq
    |> Sym.gen ~id
    |> Syntax.name loc


(* Assertions *)

let assert_parses ~ctxt parse assert_equal env input expected =
  let lexbuf =
    input
      |> String.concat "\n"
      |> Sedlexing.Utf8.from_string
  in
  ignore begin
    parse lexbuf env (fun _ actual ->
      assert_equal ~ctxt expected actual)
  end

let assert_parses_ty_annot = assert_parses Parse.parse_annot SyntaxTest.assert_ty_equal
let assert_parses_ty = assert_parses Parse.parse_ty SyntaxTest.assert_ty_equal
let assert_parses_lit = assert_parses Parse.parse_lit SyntaxTest.assert_expr_equal
let assert_parses_ident = assert_parses Parse.parse_ident SyntaxTest.assert_expr_equal
let assert_parses_patt = assert_parses Parse.parse_patt SyntaxTest.assert_patt_equal
let assert_parses_binding = assert_parses Parse.parse_binding SyntaxTest.assert_binding_equal
let assert_parses_top = assert_parses Parse.parse_top SyntaxTest.assert_top_equal
let assert_parses_name = assert_parses Parse.parse_name SyntaxTest.assert_name_equal
let assert_parses_local = assert_parses Parse.parse_local SyntaxTest.assert_name_equal

(* Annotations *)

let test_ty_annot ctxt =
  let id = "TestConstructor" in
  let env = EnvTest.fresh () in
  fresh_ty_constr ~start:(0, 2, 2) ~id ()
    |> assert_parses_ty_annot ~ctxt env [
         sprintf ": %s" id
       ]

(* Types *)

let test_ty_constr ctxt =
  let id = "TestConstructor" in
  let env = EnvTest.fresh () in
  fresh_ty_constr ~id ()
    |> assert_parses_ty ~ctxt env [id]

(* Literals *)

let assert_lit ~ctxt lexeme constr value =
  let env = EnvTest.fresh () in
  let loc =
    let len = String.length lexeme in
    LocTest.make (0, 0, 0) (0, len, len)
  in
  constr loc value
    |> assert_parses_lit ~ctxt env [lexeme]

let test_lit_bool ctxt =
  let env = EnvTest.fresh () in
  fresh_bool ~value:true ()
    |> assert_parses_lit ~ctxt env ["true"];
  fresh_bool ~value:false ()
    |> assert_parses_lit ~ctxt env ["false"]

let test_lit_int ctxt =
  let assert_int lexeme = assert_lit ~ctxt lexeme Syntax.expr_int lexeme in
  List.iter assert_int [
    "42"; "42i"; "42I";
    "-42"; "-42i"; "-42I";
    "+42"; "+42i"; "+42I";
    "0b101010"; "0b101010i"; "0b101010I";
    "-0b101010"; "-0b101010i"; "-0b101010I";
    "+0b101010"; "+0b101010i"; "+0b101010I";
    "0B101010"; "0B101010i"; "0B101010I";
    "-0B101010"; "-0B101010i"; "-0B101010I";
    "+0B101010"; "+0B101010i"; "+0B101010I";
    "0o52"; "0o52i"; "0o52I";
    "-0o52"; "-0o52i"; "-0o52I";
    "+0o52"; "+0o52i"; "+0o52I";
    "0O52"; "0O52i"; "0O52I";
    "-0O52"; "-0O52i"; "-0O52I";
    "+0O52"; "+0O52i"; "+0O52I";
    "0x2a"; "0x2ai"; "0x2aI";
    "-0x2a"; "-0x2ai"; "-0x2aI";
    "+0x2a"; "+0x2ai"; "+0x2aI";
    "0x2A"; "0x2Ai"; "0x2AI";
    "-0x2A"; "-0x2Ai"; "-0x2AI";
    "+0x2A"; "+0x2Ai"; "+0x2AI";
    "0X2a"; "0X2ai"; "0X2aI";
    "-0X2a"; "-0X2ai"; "-0X2aI";
    "+0X2a"; "+0X2ai"; "+0X2aI";
    "0X2A"; "0X2Ai"; "0X2AI";
    "-0X2A"; "-0X2Ai"; "-0X2AI";
    "+0X2A"; "+0X2Ai"; "+0X2AI";
  ]

let test_lit_long ctxt =
  let assert_long lexeme = assert_lit ~ctxt lexeme Syntax.expr_long lexeme in
  List.iter assert_long [
    "42l"; "42L";
    "-42l"; "-42L";
    "+42l"; "+42L";
    "0b101010l"; "0b101010L";
    "-0b101010l"; "-0b101010L";
    "+0b101010l"; "+0b101010L";
    "0B101010l"; "0B101010L";
    "-0B101010l"; "-0B101010L";
    "+0B101010l"; "+0B101010L";
    "0o52l"; "0o52L";
    "-0o52l"; "-0o52L";
    "+0o52l"; "+0o52L";
    "0O52l"; "0O52L";
    "-0O52l"; "-0O52L";
    "+0O52l"; "+0O52L";
    "0x2al"; "0x2aL";
    "-0x2al"; "-0x2aL";
    "+0x2al"; "+0x2aL";
    "0x2Al"; "0x2AL";
    "-0x2Al"; "-0x2AL";
    "+0x2Al"; "+0x2AL";
    "0X2al"; "0X2aL";
    "-0X2al"; "-0X2aL";
    "+0X2al"; "+0X2aL";
    "0X2Al"; "0X2AL";
    "-0X2Al"; "-0X2AL";
    "+0X2Al"; "+0X2AL";
  ]

let test_lit_float ctxt =
  let assert_float lexeme = assert_lit ~ctxt lexeme Syntax.expr_float lexeme in
  List.iter assert_float [
    "4.2"; "+4.2"; "-4.2";
    "4.2e42"; "+4.2e42"; "-4.2e42";
    "4.2e+42"; "+4.2e+42"; "-4.2e+42";
    "4.2e-42"; "+4.2e-42"; "-4.2e-42";
    "4.2E42"; "+4.2E42"; "-4.2E42";
    "4.2E+42"; "+4.2E+42"; "-4.2E+42";
    "4.2E-42"; "+4.2E-42"; "-4.2E-42";
    "4.2f"; "+4.2f"; "-4.2f";
    "4.2e42f"; "+4.2e42f"; "-4.2e42f";
    "4.2e+42f"; "+4.2e+42f"; "-4.2e+42f";
    "4.2e-42f"; "+4.2e-42f"; "-4.2e-42f";
    "4.2E42f"; "+4.2E42f"; "-4.2E42f";
    "4.2E+42f"; "+4.2E+42f"; "-4.2E+42f";
    "4.2E-42f"; "+4.2E-42f"; "-4.2E-42f";
    "4.2F"; "+4.2F"; "-4.2F";
    "4.2e42F"; "+4.2e42F"; "-4.2e42F";
    "4.2e+42F"; "+4.2e+42F"; "-4.2e+42F";
    "4.2e-42F"; "+4.2e-42F"; "-4.2e-42F";
    "4.2E42F"; "+4.2E42F"; "-4.2E42F";
    "4.2E+42F"; "+4.2E+42F"; "-4.2E+42F";
    "4.2E-42F"; "+4.2E-42F"; "-4.2E-42F";
  ]

let test_lit_double ctxt =
  let assert_double lexeme = assert_lit ~ctxt lexeme Syntax.expr_double lexeme in
  List.iter assert_double [
    "4.2d"; "+4.2d"; "-4.2d";
    "4.2e42d"; "+4.2e42d"; "-4.2e42d";
    "4.2e+42d"; "+4.2e+42d"; "-4.2e+42d";
    "4.2e-42d"; "+4.2e-42d"; "-4.2e-42d";
    "4.2E42d"; "+4.2E42d"; "-4.2E42d";
    "4.2E+42d"; "+4.2E+42d"; "-4.2E+42d";
    "4.2E-42d"; "+4.2E-42d"; "-4.2E-42d";
    "4.2D"; "+4.2D"; "-4.2D";
    "4.2e42D"; "+4.2e42D"; "-4.2e42D";
    "4.2e+42D"; "+4.2e+42D"; "-4.2e+42D";
    "4.2e-42D"; "+4.2e-42D"; "-4.2e-42D";
    "4.2E42D"; "+4.2E42D"; "-4.2E42D";
    "4.2E+42D"; "+4.2E+42D"; "-4.2E+42D";
    "4.2E-42D"; "+4.2E-42D"; "-4.2E-42D";
  ]

let test_lit_rune ctxt =
  assert_lit ~ctxt "'a'" Syntax.expr_rune (Uchar.of_char 'a');
  (* assert_lit ~ctxt "'ß'" Syntax.expr_rune (Uchar.of_char 'ß'); *)
  assert_lit ~ctxt "'\\''" Syntax.expr_rune (Uchar.of_char '\'')

let test_lit_string ctxt =
  let assert_str lexeme value =
    let env = EnvTest.fresh () in
    let loc =
      let len = Utf8.len lexeme in
      LocTest.make (0, 0, 0) (0, len, len)
    in
    value
      |> Utf8.normalize
      |> Syntax.expr_string loc
      |> assert_parses_lit ~ctxt env [lexeme]
  in
  assert_str "\"\"" "";
  assert_str "\"Grüß Gott\"" "Grüß Gott";
  assert_str "\"Straße\"" "Straße";
  assert_str "\"foo bar\"" "foo bar";
  assert_str "\"foo\\\"bar\"" "foo\"bar"

(* Identifiers *)

let assert_ident ~ctxt id =
  let env = EnvTest.fresh () in
  fresh_ident ~id ()
    |> assert_parses_ident ~ctxt env [id]

let test_ident_lident ctxt =
  List.iter (assert_ident ~ctxt) [
    "foo"; "fooBar"; "foo_bar"; "fooBar_42";
    "grüßGott"; "straße"
  ]

let test_ident_uident ctxt =
  List.iter (assert_ident ~ctxt) [
    "Foo"; "FooBar"; "Foo_bar"; "FooBar_42";
    "GrüßGott"; "Straße"
  ]

(* Patterns *)

let test_patt_ground ctxt =
  let env = EnvTest.fresh () in
  fresh_patt_ground ()
    |> assert_parses_patt ~ctxt env ["_"]

let test_patt_var ctxt =
  let assert_patt id =
    let env = EnvTest.fresh () in
    fresh_patt_var ~id ()
      |> assert_parses_patt ~ctxt env [id]
  in
  assert_patt "foobar";
  assert_patt "fooBar";
  assert_patt "foo_bar";
  assert_patt "foo_bar42";
  assert_patt "grüßGott";
  assert_patt "straße"

(* Bindings *)

let test_binding_value_binding_implicit ctxt =
  let value = fresh_bool ~start:(0, 4, 4) ~value:true () in
  let patt = fresh_patt_ground () in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (0, 0, 0) (0, 8, 8) in
  Syntax.value_binding loc patt None value
    |> assert_parses_binding ~ctxt env ["_ = true"]

let test_binding_value_binding_explicit ctxt =
  let id = "testIdent" in
  let constr = "TestConstructor" in

  let seq = Sym.seq () in
  let patt = fresh_patt_var ~seq ~id () in
  let ty = fresh_ty_constr ~start:(0, 11, 11) ~seq ~id:constr () in
  let value = fresh_bool ~start:(0, 29, 29) ~value:true () in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (0, 0, 0) (0, 33, 33) in
  Syntax.value_binding loc patt (Some ty) value
    |> assert_parses_binding ~ctxt env [
         sprintf "%s: %s = true" id constr
       ]

(* Top-Level Expressions *)

let test_top_let ctxt =
  let id = "testIdent" in
  let constr = "TestConstructor" in

  let seq = Sym.seq () in
  let patt = fresh_patt_var ~start:(0, 4, 4) ~seq ~id () in
  let ty = fresh_ty_constr ~start:(0, 15, 15) ~seq ~id:constr () in
  let value = fresh_bool ~start:(0, 33, 33) ~value:true () in
  let binding =
    let loc = LocTest.make (0, 4, 4) (0, 37, 37) in
    Syntax.value_binding loc patt (Some ty) value
  in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (0, 0, 0) (0, 37, 37) in
  Syntax.top_let loc binding
    |> assert_parses_top ~ctxt env [
         sprintf "let %s: %s = true" id constr
       ]

let test_top_val ctxt =
  let id = "testIdent" in
  let constr = "TestConstructor" in

  let seq = Sym.seq () in
  let patt = fresh_patt_var ~start:(0, 4, 4) ~seq ~id () in
  let ty = fresh_ty_constr ~start:(0, 15, 15) ~seq ~id:constr () in
  let value = fresh_bool ~start:(0, 33, 33) ~value:true () in
  let binding =
    let loc = LocTest.make (0, 4, 4) (0, 37, 37) in
    Syntax.value_binding loc patt (Some ty) value
  in

  let env = EnvTest.fresh () in
  let loc = LocTest.make (0, 0, 0) (0, 37, 37) in
  Syntax.top_val loc binding
    |> assert_parses_top ~ctxt env [
         sprintf "val %s: %s = true" id constr
       ]

(* Imports *)

let test_name ctxt =
  let assert_name id =
    let env = EnvTest.fresh () in
    fresh_name ~id ()
      |> assert_parses_name ~ctxt env [id]
  in
  List.iter assert_name [
    "foo"; "fooBar"; "foo_bar"; "fooBar_42";
    "grüßGott"; "straße"
  ]

let test_local ctxt =
  let assert_local id =
    let env = EnvTest.fresh () in
    fresh_name ~start:(0, 3, 3) ~id ()
      |> assert_parses_local ~ctxt env [
           sprintf "-> %s" id
         ]
  in
  List.iter assert_local [
    "foo"; "fooBar"; "foo_bar"; "fooBar_42";
    "grüßGott"; "straße"
  ]

(* Test Suite *)

let suite =
  "Parser" >::: [
    "Types" >::: [
      "Annotations"  >:: test_ty_annot;
      "Constructors" >:: test_ty_constr;
    ];
    "Literals" >::: [
      "Booleans" >:: test_lit_bool;
      "Integers" >:: test_lit_int;
      "Longs"    >:: test_lit_long;
      "Floats"   >:: test_lit_float;
      "Doubles"  >:: test_lit_double;
      "Runes"    >:: test_lit_rune;
      "Strings"  >:: test_lit_string;
    ];
    "Identifiers" >::: [
      "Lower Case" >:: test_ident_lident;
      "Upper Case" >:: test_ident_uident;
    ];
    "Patterns" >::: [
      "Ground"      >:: test_patt_ground;
      "Identifiers" >:: test_patt_var;
    ];
    "Bindings" >::: [
      "Value Bindings" >::: [
        "Implicit" >:: test_binding_value_binding_implicit;
        "Explicit" >:: test_binding_value_binding_explicit;
      ];
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
      "Val Bindings" >:: test_top_val;
    ];
    "Imports" >::: [
      "Names"       >:: test_name;
      "Local Names" >:: test_local;
    ];
  ]
