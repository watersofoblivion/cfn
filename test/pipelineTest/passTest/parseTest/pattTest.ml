(* Patterns *)

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_patt_ground ?start:(start = ParseUtils.bof) _ =
  let loc = ParseUtils.lexeme_loc start "_" in
  SyntaxTest.fresh_patt_ground ~loc ()

let fresh_patt_var ?start:(start = ParseUtils.bof) ?id:(id = SymTest.fresh_sym ()) _ =
  let loc = ParseUtils.sym_loc start id in
  SyntaxTest.fresh_patt_var ~loc ~id ()

(* Assertions *)

let assert_parses_patt = ParseUtils.assert_parses Parse.parse_patt SyntaxTest.assert_patt_equal

(* Tests *)

let test_parse_patt_ground ctxt =
  fresh_patt_ground ()
    |> assert_parses_patt ~ctxt ["_"]

let test_parse_patt_var ctxt =
  let lexeme = "testId" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_patt_var ~id ()
    |> assert_parses_patt ~ctxt [lexeme]

(* Test Suite *)

let suite =
  "Patterns" >::: [
    "Ground"    >:: test_parse_patt_ground;
    "Variables" >:: test_parse_patt_var;
  ]
