open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_ty_constr ?loc:(loc = LocTest.gen ()) ?seq:(seq = Sym.seq ()) ?id:(id = Prim.id_bool) _ =
  seq
    |> Sym.gen ~id
    |> Syntax.ty_constr loc

(* Utilities *)

let deloc_ty = function
  | Syntax.TyConstr constr -> Syntax.ty_constr LocTest.dummy constr.id

(* Assertions *)

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.TyConstr expected, Syntax.TyConstr actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id

(* Tests *)

(* Constructors *)

let test_ty_constr ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Syntax.ty_constr loc id in
  match expected with
    | Syntax.TyConstr actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id

let test_constructor =
  "Constructors" >::: [
    "Type Constructor" >:: test_ty_constr;
  ]

(* Equality *)

let printer ty =
  ty
    |> fprintf str_formatter "%a" Syntax.pp_ty
    |> flush_str_formatter

let test_ty_equal_equal ctxt =
  let ty = fresh_ty_constr ~id:Prim.id_bool () in
  let ty' = fresh_ty_constr ~id:Prim.id_bool () in
  assert_equal ~ctxt ~cmp:Syntax.ty_equal ~printer ~msg:"Types are not equal" ty ty'

let test_ty_equal_not_equal ctxt =
  let ty = fresh_ty_constr ~id:Prim.id_bool () in
  let ty' = fresh_ty_constr ~id:Prim.id_int () in
  let cmp ty ty' = not (Syntax.ty_equal ty ty') in
  assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" ty ty'

let test_equal =
  "Equality" >::: [
    "Equal"     >:: test_ty_equal_equal;
    "Not Equal" >:: test_ty_equal_not_equal;
  ]

(* Location *)

let test_loc_ty_constr ctxt =
  let loc = LocTest.gen () in
  fresh_ty_constr ~loc ()
    |> Syntax.loc_ty
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc =
  "Locations" >::: [
    "Type Constructor" >:: test_loc_ty_constr;
  ]

(* Pretty Printing *)

let assert_pp_ty = PrettyTest.assert_pp Syntax.pp_ty

let test_pp_ty_constr ctxt =
  let constr =
    ()
      |> Sym.seq
      |> Sym.gen ~id:Prim.id_bool
  in
  Syntax.ty_constr LocTest.dummy constr
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_pp =
  "Pretty Printing" >::: [
    "Type Constructor" >:: test_pp_ty_constr;
  ]

(* Test Suite *)

let suite =
  "Types" >::: [
    test_constructor;
    test_equal;
    test_loc;
    test_pp;
  ]
