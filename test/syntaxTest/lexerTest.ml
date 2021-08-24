open OUnit2

open Syntax

open CommonTest

(* Assertions *)

let assert_token_equal ~ctxt expected actual = match (expected, actual) with
  | Parser.EOF, Parser.EOF
  | Parser.PIPE, Parser.PIPE
  | Parser.ARROW, Parser.ARROW
  | Parser.PACKAGE, Parser.PACKAGE
  | Parser.FROM, Parser.FROM
  | Parser.IMPORT, Parser.IMPORT -> ()
  | Parser.LIDENT expected, Parser.LIDENT actual ->
    assert_equal ~ctxt ~msg:"Lower-case identifier values are not equal" ~printer:Fun.id expected actual
  (* | Parser.STRING expected, Parser.STRING actual ->
    assert_equal ~ctxt ~msg:"String lexemes are not equal" ~printer:Fun.id expected actual *)
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

let test_lex_kwd_package ctxt =
  Lexer.kwd_package
    |> assert_lexes ~ctxt "package"

let test_lex_kwd_from ctxt =
  Lexer.kwd_from
    |> assert_lexes ~ctxt "from"

let test_lex_kwd_import ctxt =
  Lexer.kwd_import
    |> assert_lexes ~ctxt "import"

let test_lex_lit_lident ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_lident [
    "foo"; "fooBar"; "foo_bar"; "fooBar42"
  ]

(* let test_lex_string ctxt =
  assert_lex_lit ~ctxt ~printer:Fun.id Lexer.lit_string [] *)

let test_lex =
  "Tokens" >::: [
    "Non-Printable" >::: [
      "End-of-File" >:: test_lex_punct_eof;
    ];
    "Punctuation" >::: [
      "Pipe"  >:: test_lex_punct_pipe;
      "Arrow" >:: test_lex_punct_arrow;
    ];
    "Keywords" >::: [
      "Package" >:: test_lex_kwd_package;
      "From"    >:: test_lex_kwd_from;
      "Import"  >:: test_lex_kwd_import;
    ];
    "Literals" >::: [
      "Lower-Case Identifiers" >:: test_lex_lit_lident;
      (* "Strings"                >:: test_lex_lit_string; *)
    ];
  ]

(* Test Suite *)
let suite =
  "Lexer" >::: [
    test_lex
  ]
