open OUnit2

open Common
open Annot

open CommonTest

(* Assertions *)

let assert_pp_ty = PrettyTest.assert_pp Fmt.ty
let assert_pp_expr = PrettyTest.assert_pp Fmt.expr

(* Types *)

let test_ty_bool ctxt =
  Type.bool
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_ty_int ctxt =
  Type.int
    |> assert_pp_ty ~ctxt [Prim.id_int]

let test_ty_long ctxt =
  Type.long
    |> assert_pp_ty ~ctxt [Prim.id_long]

let test_ty_float ctxt =
  Type.float
    |> assert_pp_ty ~ctxt [Prim.id_float]

let test_ty_double ctxt =
  Type.double
    |> assert_pp_ty ~ctxt [Prim.id_double]

let test_ty_rune ctxt =
  Type.rune
    |> assert_pp_ty ~ctxt [Prim.id_rune]

let test_ty_string ctxt =
  Type.string
    |> assert_pp_ty ~ctxt [Prim.id_string]

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
    "Types" >::: [
      "Boolean" >:: test_ty_bool;
      "Integer" >:: test_ty_int;
      "Long"    >:: test_ty_long;
      "Float"   >:: test_ty_float;
      "Double"  >:: test_ty_double;
      "Rune"    >:: test_ty_rune;
      "String"  >:: test_ty_string;
    ];
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
