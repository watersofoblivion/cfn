open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let assert_pp_ty = PrettyTest.assert_pp Annot.pp_ty
let assert_pp_expr = PrettyTest.assert_pp Annot.pp_expr
let assert_pp_patt = PrettyTest.assert_pp Annot.pp_patt
let assert_pp_binding = PrettyTest.assert_pp Annot.pp_binding
let assert_pp_top = PrettyTest.assert_pp Annot.pp_top

(* Types *)

let test_pp_ty_bool ctxt =
  Annot.ty_bool
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_pp_ty_int ctxt =
  Annot.ty_int
    |> assert_pp_ty ~ctxt [Prim.id_int]

let test_pp_ty_long ctxt =
  Annot.ty_long
    |> assert_pp_ty ~ctxt [Prim.id_long]

let test_pp_ty_float ctxt =
  Annot.ty_float
    |> assert_pp_ty ~ctxt [Prim.id_float]

let test_pp_ty_double ctxt =
  Annot.ty_double
    |> assert_pp_ty ~ctxt [Prim.id_double]

let test_pp_ty_rune ctxt =
  Annot.ty_rune
    |> assert_pp_ty ~ctxt [Prim.id_rune]

let test_pp_ty_string ctxt =
  Annot.ty_string
    |> assert_pp_ty ~ctxt [Prim.id_string]

(* Expressions *)

let test_expr_bool ctxt =
  Annot.expr_bool true
    |> assert_pp_expr ~ctxt ["true"];
  Annot.expr_bool false
    |> assert_pp_expr ~ctxt ["false"]

let test_expr_int ctxt =
  Annot.expr_int 42l
    |> assert_pp_expr ~ctxt ["42"]

let test_expr_long ctxt =
  Annot.expr_long 42L
    |> assert_pp_expr ~ctxt ["42"]

let test_expr_float ctxt =
  Annot.expr_float 4.2
    |> assert_pp_expr ~ctxt ["4.2"]

let test_expr_double ctxt =
  Annot.expr_double 4.2
    |> assert_pp_expr ~ctxt ["4.2"]

let test_expr_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Annot.expr_rune
    |> assert_pp_expr ~ctxt ["'a'"]

let test_expr_string ctxt =
  Annot.expr_string "foo bar"
    |> assert_pp_expr ~ctxt ["\"foo bar\""]

(* Patterns *)

let test_patt_ground ctxt =
  Annot.patt_ground
    |> assert_pp_patt ~ctxt ["_"]

let test_patt_var ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> Annot.patt_var
    |> assert_pp_patt ~ctxt ["$0"]

(* Bindings *)

let test_binding ctxt =
  let patt = Annot.patt_ground in
  let ty = Annot.ty_bool in
  let value = Annot.expr_bool true in
  Annot.binding patt ty value
    |> assert_pp_binding ~ctxt [
         fprintf str_formatter "%a: %a = %a" Annot.pp_patt patt Annot.pp_ty ty Annot.pp_expr value |> flush_str_formatter
       ]

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding =
    true
      |> Annot.expr_bool
      |> Annot.binding Annot.patt_ground Annot.ty_bool
  in
  Annot.top_let binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Annot.pp_binding binding |> flush_str_formatter
       ]

(* Test Suite *)

let suite =
  "Pretty Printing" >::: [
    "Types" >::: [
      "Boolean" >:: test_pp_ty_bool;
      "Integer" >:: test_pp_ty_int;
      "Long"    >:: test_pp_ty_long;
      "Float"   >:: test_pp_ty_float;
      "Double"  >:: test_pp_ty_double;
      "Rune"    >:: test_pp_ty_rune;
      "String"  >:: test_pp_ty_string;
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
    "Patterns" >::: [
      "Ground"     >:: test_patt_ground;
      "Identifier" >:: test_patt_var;
    ];
    "Bindings" >:: test_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
    ];
  ]
