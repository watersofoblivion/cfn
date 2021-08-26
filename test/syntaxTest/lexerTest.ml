open Format

open OUnit2

open Syntax

open CommonTest

(* Assertions *)

let assert_token_equal ~ctxt expected actual = match (expected, actual) with
  | Parser.EOF, Parser.EOF
  | Parser.PIPE, Parser.PIPE
  | Parser.ARROW, Parser.ARROW
  | Parser.COLON, Parser.COLON
  | Parser.BIND, Parser.BIND
  | Parser.GROUND, Parser.GROUND
  | Parser.PACKAGE, Parser.PACKAGE
  | Parser.FROM, Parser.FROM
  | Parser.IMPORT, Parser.IMPORT
  | Parser.LET, Parser.LET
  | Parser.VAL, Parser.VAL -> ()
  | Parser.BOOL expected, Parser.BOOL actual ->
    assert_equal ~ctxt ~msg:"Boolean literals are not equal" ~printer:string_of_bool expected actual
  | Parser.INT expected, Parser.INT actual ->
    assert_equal ~ctxt ~msg:"Integer lexemes are not equal" ~printer:Fun.id expected actual
  | Parser.LONG expected, Parser.LONG actual ->
    assert_equal ~ctxt ~msg:"Long lexemes are not equal" ~printer:Fun.id expected actual
  | Parser.FLOAT expected, Parser.FLOAT actual ->
    assert_equal ~ctxt ~msg:"Float lexemes are not equal" ~printer:Fun.id expected actual
  | Parser.DOUBLE expected, Parser.DOUBLE actual ->
    assert_equal ~ctxt ~msg:"Double lexemes are not equal" ~printer:Fun.id expected actual
  | Parser.RUNE expected, Parser.RUNE actual ->
    let printer r = sprintf "%c" (Uchar.to_char r) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~msg:"Rune literals are not equal" ~printer expected actual
  | Parser.STRING expected, Parser.STRING actual ->
    let cmp s s' = List.fold_left2 (fun acc c c' -> acc && Uchar.equal c c') true s s' in
    let printer s =
      s
        |> List.map Uchar.to_char
        |> List.map (sprintf "%c")
        |> String.concat ""
    in
    assert_equal ~ctxt ~cmp ~msg:"String literals are not equal" ~printer expected actual
  | Parser.LIDENT expected, Parser.LIDENT actual ->
    assert_equal ~ctxt ~msg:"Lower-case identifier values are not equal" ~printer:Fun.id expected actual
  | Parser.UIDENT expected, Parser.UIDENT actual ->
    assert_equal ~ctxt ~msg:"Upper-case identifier values are not equal" ~printer:Fun.id expected actual
  | _ -> assert_failure "Tokens are not equal"

let assert_lexes = LexTest.assert_lexes Lexer.lex assert_token_equal
let assert_lex_lit = LexTest.assert_lex_lit Lexer.lex assert_token_equal

(* Lexer *)

let test_lex_punct_eof ctxt =
  Lexer.punct_eof
    |> assert_lexes ~ctxt ""

let test_lex_punct_pipe ctxt =
  Lexer.punct_pipe
    |> assert_lexes ~ctxt "|"

let test_lex_punct_arrow ctxt =
  Lexer.punct_arrow
    |> assert_lexes ~ctxt "->"

let test_lex_punct_colon ctxt =
  Lexer.punct_colon
    |> assert_lexes ~ctxt ":"

let test_lex_punct_bind ctxt =
  Lexer.punct_bind
    |> assert_lexes ~ctxt "="

let test_lex_punct_ground ctxt =
  Lexer.punct_ground
    |> assert_lexes ~ctxt "_"

let test_lex_kwd_package ctxt =
  Lexer.kwd_package
    |> assert_lexes ~ctxt "package"

let test_lex_kwd_from ctxt =
  Lexer.kwd_from
    |> assert_lexes ~ctxt "from"

let test_lex_kwd_import ctxt =
  Lexer.kwd_import
    |> assert_lexes ~ctxt "import"

let test_lex_kwd_let ctxt =
  Lexer.kwd_let
    |> assert_lexes ~ctxt "let"

let test_lex_kwd_val ctxt =
  Lexer.kwd_val
    |> assert_lexes ~ctxt "val"

let test_lex_lit_bool ctxt =
  Lexer.lit_bool true
    |> assert_lexes ~ctxt "true";
  Lexer.lit_bool false
    |> assert_lexes ~ctxt "false"

let test_lex_lit_int ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_int [
    "10"; "-10"; "+10";
    "10i"; "-10i"; "+10i";
    "10I"; "-10I"; "+10I";
    "0b1010"; "0B1010"; "-0b1010"; "-0B1010"; "+0b1010"; "+0B1010";
    "0b1010i"; "0B1010i"; "-0b1010i"; "-0B1010i"; "+0b1010i"; "+0B1010i";
    "0b1010I"; "0B1010I"; "-0b1010I"; "-0B1010I"; "+0b1010I"; "+0B1010I";
    "0o12"; "0O12"; "-0o12"; "-0O12"; "+0o12"; "+0O12";
    "0o12i"; "0O12i"; "-0o12i"; "-0O12i"; "+0o12i"; "+0O12i";
    "0o12I"; "0O12I"; "-0o12I"; "-0O12I"; "+0o12I"; "+0O12I";
    "0xA"; "0XA"; "-0xA"; "-0XA"; "+0xA"; "+0XA";
    "0xa"; "0Xa"; "-0xa"; "-0Xa"; "+0xa"; "+0Xa";
    "0xAi"; "0XAi"; "-0xAi"; "-0XAi"; "+0xAi"; "+0XAi";
    "0xai"; "0Xai"; "-0xai"; "-0Xai"; "+0xai"; "+0Xai";
    "0xAI"; "0XAI"; "-0xAI"; "-0XAI"; "+0xAI"; "+0XAI";
    "0xaI"; "0XaI"; "-0xaI"; "-0XaI"; "+0xaI"; "+0XaI";
  ]

let test_lex_lit_long ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_long [
    "10l"; "-10l"; "+10l";
    "10L"; "-10L"; "+10L";
    "0b1010l"; "0B1010l"; "-0b1010l"; "-0B1010l"; "+0b1010l"; "+0B1010l";
    "0b1010L"; "0B1010L"; "-0b1010L"; "-0B1010L"; "+0b1010L"; "+0B1010L";
    "0o12l"; "0O12l"; "-0o12l"; "-0O12l"; "+0o12l"; "+0O12l";
    "0o12L"; "0O12L"; "-0o12L"; "-0O12L"; "+0o12L"; "+0O12L";
    "0xAl"; "0XAl"; "-0xAl"; "-0XAl"; "+0xAl"; "+0XAl";
    "0xal"; "0Xal"; "-0xal"; "-0Xal"; "+0xal"; "+0Xal";
    "0xAL"; "0XAL"; "-0xAL"; "-0XAL"; "+0xAL"; "+0XAL";
    "0xaL"; "0XaL"; "-0xaL"; "-0XaL"; "+0xaL"; "+0XaL";
  ]

let test_lex_lit_float ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_float [
    "1.0"; "-1.0"; "+1.0";
    "1.0f"; "-1.0f"; "+1.0f";
    "1.0F"; "-1.0F"; "+1.0F";
    "1.0e1"; "-1.0e1"; "1.0e-1"; "-1.0e-1";
    "1.0e1"; "+1.0e1"; "1.0e+1"; "+1.0e+1";
    "1.0e1"; "-1.0e1"; "1.0e-1"; "-1.0e-1";
    "1.0e1f"; "-1.0e1f"; "1.0e-1f"; "-1.0e-1f";
    "1.0e1F"; "-1.0e1F"; "1.0e-1F"; "-1.0e-1F";
    "1.0E1"; "-1.0E1"; "1.0E-1"; "-1.0E-1";
    "1.0E1f"; "-1.0E1f"; "1.0E-1f"; "-1.0E-1f";
    "1.0E1F"; "-1.0E1F"; "1.0E-1F"; "-1.0E-1F";
  ]

let test_lex_lit_double ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_double [
    "1.0d"; "-1.0d"; "+1.0d";
    "1.0D"; "-1.0D"; "+1.0D";
    "1.0e1d"; "+1.0e1d"; "1.0e+1d"; "+1.0e+1d";
    "1.0e1D"; "-1.0e1D"; "1.0e-1D"; "-1.0e-1D";
    "1.0e1d"; "-1.0e1d"; "1.0e-1d"; "-1.0e-1d";
    "1.0e1D"; "-1.0e1D"; "1.0e-1D"; "-1.0e-1D";
    "1.0E1d"; "-1.0E1d"; "1.0E-1d"; "-1.0E-1d";
    "1.0E1D"; "-1.0E1D"; "1.0E-1D"; "-1.0E-1D";
  ]

let test_lex_lit_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Lexer.lit_rune
    |> assert_lexes ~ctxt "'a'";
  '\''
    |> Uchar.of_char
    |> Lexer.lit_rune
    |> assert_lexes ~ctxt "'\\''"

let test_lex_lit_string ctxt =
  Lexer.lit_string ""
    |> assert_lexes ~ctxt "\"\"";
  Lexer.lit_string "foo bar"
    |> assert_lexes ~ctxt "\"foo bar\"";
  Lexer.lit_string "foo\"bar"
    |> assert_lexes ~ctxt "\"foo\\\"bar\""

let test_lex_lit_lident ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_lident [
    "foo"; "fooBar"; "foo_bar"; "fooBar42";
  ]

let test_lex_lit_uident ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_uident [
    "Foo"; "FooBar"; "Foo_bar"; "FooBar42";
  ]

let test_lex =
  "Tokens" >::: [
    "Non-Printable" >::: [
      "End-of-File" >:: test_lex_punct_eof;
    ];
    "Punctuation" >::: [
      "Pipe"   >:: test_lex_punct_pipe;
      "Arrow"  >:: test_lex_punct_arrow;
      "Colon"  >:: test_lex_punct_colon;
      "Bind"   >:: test_lex_punct_bind;
      "Ground" >:: test_lex_punct_ground;
    ];
    "Keywords" >::: [
      "Package" >:: test_lex_kwd_package;
      "From"    >:: test_lex_kwd_from;
      "Import"  >:: test_lex_kwd_import;
      "Let"     >:: test_lex_kwd_let;
      "Val"     >:: test_lex_kwd_val;
    ];
    "Literals" >::: [
      "Booleans"               >:: test_lex_lit_bool;
      "Integers"               >:: test_lex_lit_int;
      "Longs"                  >:: test_lex_lit_long;
      "Floats"                 >:: test_lex_lit_float;
      "Doubles"                >:: test_lex_lit_double;
      "Runes"                  >:: test_lex_lit_rune;
      "Strings"                >:: test_lex_lit_string;
      "Lower-Case Identifiers" >:: test_lex_lit_lident;
      "Upper-Case Identifiers" >:: test_lex_lit_uident;
    ];
  ]

(* Test Suite *)
let suite =
  "Lexer" >::: [
    test_lex
  ]
