(* Patterns *)

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_patt_ground _ =
  Mono.patt_ground

let fresh_patt_var ?id:(id = SymTest.fresh_sym ()) _ =
  Mono.patt_var id

(* Assertions *)

let patt_not_equal = TestUtils.not_equal "Patterns" Mono.pp_patt

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.PattGround, Mono.PattGround -> ()
  | Mono.PattVar expected, Mono.PattVar actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_patt_ground ctxt =
  let expected = Mono.patt_ground in
  match expected with
    | Mono.PattGround -> ()
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Mono.patt_var id in
  match expected with
    | Mono.PattVar actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

let test_constructors =
  "Constructors" >::: [
    "Ground"    >:: test_patt_ground;
    "Variables" >:: test_patt_var;
  ]

(* Pretty Printing *)

let assert_pp_patt = PrettyTest.assert_pp Mono.pp_patt


let test_pp_patt_ground ctxt =
  Mono.patt_ground
    |> assert_pp_patt ~ctxt ["_"]

let test_pp_patt_var ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> Mono.patt_var
    |> assert_pp_patt ~ctxt ["$0"]

let test_pp =
  "Pretty Printing" >::: [
    "Ground"    >:: test_pp_patt_ground;
    "Variables" >:: test_pp_patt_var;
  ]

(* Type Checking *)


let test_check_patt_ground _ =
  let env = EnvTest.fresh () in
  let patt = Mono.patt_ground in
  let ty = Mono.ty_bool in
  Mono.check_patt env patt ty (fun _ -> ())

let test_check_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Mono.patt_var id in
  let ty = Mono.ty_bool in
  Mono.check_patt env patt ty (fun env ->
    MonoUtils.assert_ty_bound ~ctxt id env ty)

let test_check =
  "Type Checking" >::: [
    "Ground"    >:: test_check_patt_ground;
    "Variables" >:: test_check_patt_var;
  ]

(* Test Suite *)

let suite =
  "Patterns" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
