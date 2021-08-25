open OUnit2

open Mono

open CommonTest

(* Assertions *)

let assert_pp_atom = PrettyTest.assert_pp Fmt.atom
let assert_pp_expr = PrettyTest.assert_pp Fmt.expr
let assert_pp_block = PrettyTest.assert_pp Fmt.block

(* Atoms *)

let test_atom_bool ctxt =
  Ast.atom_bool true
    |> assert_pp_atom ~ctxt ["true"];
  Ast.atom_bool false
    |> assert_pp_atom ~ctxt ["false"]

let test_atom_int ctxt =
  Ast.atom_int 42l
    |> assert_pp_atom ~ctxt ["42"]

let test_atom_long ctxt =
  Ast.atom_long 42L
    |> assert_pp_atom ~ctxt ["42"]

let test_atom_float ctxt =
  Ast.atom_float 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_atom_double ctxt =
  Ast.atom_double 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_atom_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Ast.atom_rune
    |> assert_pp_atom ~ctxt ["'a'"]

let test_atom_string ctxt =
  "foo bar"
    |> String.to_seq
    |> List.of_seq
    |> List.map Uchar.of_char
    |> Ast.atom_string
    |> assert_pp_atom ~ctxt ["\"foo bar\""]

(* Expressions *)

let test_expr_atom ctxt =
  true
    |> Ast.atom_bool
    |> Ast.expr_atom
    |> assert_pp_expr ~ctxt ["true"]

(* Blocks *)

let test_block_expr ctxt =
  true
    |> Ast.atom_bool
    |> Ast.expr_atom
    |> Ast.block_expr
    |> assert_pp_block ~ctxt ["true"]

(* Test Suite *)

let suite =
  "Pretty Printing" >::: [
    "Atoms" >::: [
      "Booleans" >:: test_atom_bool;
      "Integers" >:: test_atom_int;
      "Longs"    >:: test_atom_long;
      "Floats"   >:: test_atom_float;
      "Doubles"  >:: test_atom_double;
      "Runes"    >:: test_atom_rune;
      "Strings"  >:: test_atom_string;
    ];
    "Expressions" >::: [
      "Atoms" >:: test_expr_atom;
    ];
    "Blocks" >::: [
      "Expressions" >:: test_block_expr;
    ];
  ]
