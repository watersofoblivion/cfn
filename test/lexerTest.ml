open Format

open OUnit2

open Cfn

(* Assertions *)

let assert_lexes ~ctxt lex str expected =
  let open Parser in

  let cmp tok tok' =
    match (tok, tok') with
    | EOF loc, EOF loc'
    | PIPE loc, PIPE loc'
    | ARROW loc, ARROW loc'
    | PACKAGE loc, PACKAGE loc'
    | FROM loc, FROM loc'
    | IMPORT loc, IMPORT loc' -> loc = loc'
    | LIDENT (loc, id), LIDENT (loc', id') -> loc = loc' && id = id'
    | STRING (loc, str), STRING (loc', str') -> loc = loc' && str = str'
    | _ -> false
  in

  let pp_diff fmt (expected, actual) =
    let print_loc fmt loc =
      fprintf fmt "[%S (%d): (%d, %d, %d) - (%d, %d, %d)]"
        loc.Loc.fname
        loc.length
        loc.start_pos.line loc.start_pos.col loc.start_pos.off
        loc.end_pos.line loc.end_pos.col loc.end_pos.off
    in
    let print_tok fmt = function
      | EOF loc -> fprintf fmt "<eof> @ %a" print_loc loc
      | PIPE loc -> fprintf fmt "| @ %a" print_loc loc
      | ARROW loc -> fprintf fmt "-> @ %a" print_loc loc
      | PACKAGE loc -> fprintf fmt "package @ %a" print_loc loc
      | FROM loc -> fprintf fmt "from @ %a" print_loc loc
      | IMPORT loc -> fprintf fmt "import @ %a" print_loc loc
      | LIDENT(loc, id) -> fprintf fmt "%s @ %a" id print_loc loc
      | STRING(loc, str) -> fprintf fmt "%S @ %a" str print_loc loc
    in
    fprintf fmt "Expected:\n%a\nActual:\n%a" print_tok expected print_tok actual
  in

  let lexbuf = Lexing.from_string str in
  let actual = lex lexbuf in
  assert_equal ~ctxt ~cmp ~pp_diff expected actual

(* Lexer *)
let test_lex =
  let assert_lexes ~ctxt str tok = assert_lexes ~ctxt Lexer.lex str tok in

  let test_non_printable =
    let test_eof ctxt =
      let loc = Loc.mock "" (1, 0, 0) (1, 0, 0) in
      let expected = Parser.EOF loc in
      assert_lexes ~ctxt "" expected
    in
    "Non-Printable Tokens" >::: [
      "End of File" >:: test_eof
    ]
  in
  let test_punctuation =
    let test_pipe ctxt =
      let loc = Loc.mock "" (1, 0, 0) (1, 1, 1) in
      let expected = Parser.PIPE loc in
      assert_lexes ~ctxt "|" expected
    in
    let test_arrow ctxt =
      let loc = Loc.mock "" (1, 0, 0) (1, 2, 2) in
      let expected = Parser.ARROW loc in
      assert_lexes ~ctxt "->" expected
    in
    "Punctuation" >::: [
      "Pipe"  >:: test_pipe;
      "Arrow" >:: test_arrow
    ]
  in
  let test_keywords =
    let test_package ctxt =
      let loc = Loc.mock "" (1, 0, 0) (1, 7, 7) in
      let expected = Parser.PACKAGE loc in
      assert_lexes ~ctxt "package" expected
    in
    let test_from ctxt =
      let loc = Loc.mock "" (1, 0, 0) (1, 4, 4) in
      let expected = Parser.FROM loc in
      assert_lexes ~ctxt "from" expected
    in
    let test_import ctxt =
      let loc = Loc.mock "" (1, 0, 0) (1, 6, 6) in
      let expected = Parser.IMPORT loc in
      assert_lexes ~ctxt "import" expected
    in
    "Keywords" >::: [
      "Package" >:: test_package;
      "From"    >:: test_from;
      "Import"  >:: test_import;
    ]
  in
  let test_syntactic =
    let test_lident ctxt =
      let id = "foo" in
      let loc = Loc.mock "" (1, 0, 0) (1, 3, 3) in
      let expected = Parser.LIDENT(loc, id) in
      assert_lexes ~ctxt id expected
    in
    let test_string ctxt =
      let test str =
        let lit = sprintf "%S" str in
        let len = String.length lit in
        let loc = Loc.mock "" (1, 0, 0) (1, len, len) in
        let expected = Parser.STRING(loc, str) in
        assert_lexes ~ctxt lit expected
      in
      List.iter test ["foo"; "foo\"bar"]
    in
    "Syntactic Values" >::: [
      "Lower-Case Identifiers" >:: test_lident;
      "Strings"                >:: test_string
    ]
  in
  "Lex" >::: [
    test_non_printable;
    test_punctuation;
    test_keywords;
    test_syntactic
  ]

(* Test Suite *)
let suite =
  "Lexer" >::: [
    test_lex
  ]
