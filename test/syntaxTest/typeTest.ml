open OUnit2

open Common
open Syntax

open CommonTest

(* Assertions *)

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Type.Constr expected, Type.Constr actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id

(* Constructors *)

let test_constr ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Type.constr loc id in
  match expected with
    | Type.Constr actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id

(* Test Suite *)

let suite =
  "Types" >::: [
    "Constructors" >::: [
      "Constructor" >:: test_constr;
    ];
  ]
