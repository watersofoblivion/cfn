open Common

open OUnit2

(* Assertions *)

let assert_lexes ~ctxt lex assert_equal input expected =
  input
    |> Lex.from_string
    |> lex
    |> assert_equal ~ctxt expected

let rec assert_lex_lit_pair ~ctxt lex assert_equal construct = function
  | [] -> ()
  | (value, str) :: pairs ->
    value
      |> construct
      |> assert_lexes ~ctxt lex assert_equal str;
    assert_lex_lit_pair ~ctxt lex assert_equal construct pairs

let assert_lex_lit ~ctxt ~printer lex assert_equal construct values =
  let map x = (x, printer x) in
  values
    |> List.map map
    |> assert_lex_lit_pair ~ctxt lex assert_equal construct

(* Tests *)

(* Suite *)

let suite =
  "Lexing" >::: [
  ]
