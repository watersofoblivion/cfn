open OUnit2

open Annot

open CommonTest

(* Assertions *)

let assert_pp_expr = PrettyTest.assert_pp Fmt.expr

(* Expressions *)

let test_expr_bool ctxt =
  Ast.bool true
    |> assert_pp_expr ~ctxt ["true"];
  Ast.bool false
    |> assert_pp_expr ~ctxt ["false"]

let test_expr_int ctxt =
  Ast.int 42l
    |> assert_pp_expr ~ctxt ["42"]

let test_expr_long ctxt =
  Ast.long 42L
    |> assert_pp_expr ~ctxt ["42"]

let test_expr_float ctxt =
  Ast.float 4.2
    |> assert_pp_expr ~ctxt ["4.2"]

let test_expr_double ctxt =
  Ast.double 4.2
    |> assert_pp_expr ~ctxt ["4.2"]

let test_expr_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Ast.rune
    |> assert_pp_expr ~ctxt ["'a'"]

let test_expr_string ctxt =
  "foo bar"
    |> String.to_seq
    |> List.of_seq
    |> List.map Uchar.of_char
    |> Ast.string
    |> assert_pp_expr ~ctxt ["\"foo bar\""]

(* Test Suite *)

let suite =
  "Pretty Printing" >::: [
    "Expressions" >::: [
      "Booleans" >:: test_expr_bool;
      "Integers" >:: test_expr_int;
      "Longs"    >:: test_expr_long;
      "Floats"   >:: test_expr_float;
      "Doubles"  >:: test_expr_double;
      "Runes"    >:: test_expr_rune;
      "Strings"  >:: test_expr_string;
    ];
  ]
