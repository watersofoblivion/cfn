open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let assert_pp_ty = PrettyTest.assert_pp Clos.pp_ty
let assert_pp_builtin = PrettyTest.assert_pp Clos.pp_builtin
let assert_pp_atom = PrettyTest.assert_pp Clos.pp_atom
let assert_pp_expr = PrettyTest.assert_pp Clos.pp_expr
let assert_pp_block = PrettyTest.assert_pp Clos.pp_block
let assert_pp_patt = PrettyTest.assert_pp Clos.pp_patt
let assert_pp_binding = PrettyTest.assert_pp Clos.pp_binding
let assert_pp_top = PrettyTest.assert_pp Clos.pp_top

(* Types *)

let test_ty_bool ctxt =
  Clos.ty_bool
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_ty_int ctxt =
  Clos.ty_int
    |> assert_pp_ty ~ctxt [Prim.id_int]

let test_ty_long ctxt =
  Clos.ty_long
    |> assert_pp_ty ~ctxt [Prim.id_long]

let test_ty_float ctxt =
  Clos.ty_float
    |> assert_pp_ty ~ctxt [Prim.id_float]

let test_ty_double ctxt =
  Clos.ty_double
    |> assert_pp_ty ~ctxt [Prim.id_double]

let test_ty_rune ctxt =
  Clos.ty_rune
    |> assert_pp_ty ~ctxt [Prim.id_rune]

let test_ty_string ctxt =
  Clos.ty_string
    |> assert_pp_ty ~ctxt [Prim.id_string]

(* Atoms *)

let test_atom_bool ctxt =
  Clos.atom_bool true
    |> assert_pp_atom ~ctxt ["true"];
  Clos.atom_bool false
    |> assert_pp_atom ~ctxt ["false"]

let test_atom_int ctxt =
  Clos.atom_int 42l
    |> assert_pp_atom ~ctxt ["42"]

let test_atom_long ctxt =
  Clos.atom_long 42L
    |> assert_pp_atom ~ctxt ["42"]

let test_atom_float ctxt =
  Clos.atom_float 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_atom_double ctxt =
  Clos.atom_double 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_atom_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Clos.atom_rune
    |> assert_pp_atom ~ctxt ["'a'"]

let test_atom_string ctxt =
  "foo bar"
    |> Clos.atom_string
    |> assert_pp_atom ~ctxt ["\"foo bar\""]

let test_atom_ident ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> Clos.atom_ident
    |> assert_pp_atom ~ctxt ["$0"]

(* Expressions *)

let test_expr_atom ctxt =
  true
    |> Clos.atom_bool
    |> Clos.expr_atom
    |> assert_pp_expr ~ctxt ["true"]

(* Blocks *)

let test_block_expr ctxt =
  true
    |> Clos.atom_bool
    |> Clos.expr_atom
    |> Clos.block_expr
    |> assert_pp_block ~ctxt ["true"]

(* Patterns *)

let test_patt_ground ctxt =
  Clos.patt_ground
    |> assert_pp_patt ~ctxt ["_"]

let test_patt_var ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> Clos.patt_var
    |> assert_pp_patt ~ctxt ["$0"]

(* Bindings *)

let test_binding ctxt =
  let patt = Clos.patt_ground in
  let ty = Clos.ty_bool in
  let value =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
  in
  Clos.binding patt ty value
    |> assert_pp_binding ~ctxt [
         fprintf str_formatter "%a: %a = %a" Clos.pp_patt patt Clos.pp_ty ty Clos.pp_expr value |> flush_str_formatter
       ]

let test_top_let ctxt =
  let binding =
    let patt = Clos.patt_ground in
    let ty = Clos.ty_bool in
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding patt ty
  in
  Clos.top_let binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Clos.pp_binding binding |> flush_str_formatter
       ]

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
    "Atoms" >::: [
      "Booleans"    >:: test_atom_bool;
      "Integers"    >:: test_atom_int;
      "Longs"       >:: test_atom_long;
      "Floats"      >:: test_atom_float;
      "Doubles"     >:: test_atom_double;
      "Runes"       >:: test_atom_rune;
      "Strings"     >:: test_atom_string;
      "Identifiers" >:: test_atom_ident;
    ];
    "Expressions" >::: [
      "Atoms" >:: test_expr_atom;
    ];
    "Blocks" >::: [
      "Expressions" >:: test_block_expr;
    ];
    "Patterns" >::: [
      "Ground"   >:: test_patt_ground;
      "Variable" >:: test_patt_var;
    ];
    "Bindings" >:: test_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
    ];
  ]
