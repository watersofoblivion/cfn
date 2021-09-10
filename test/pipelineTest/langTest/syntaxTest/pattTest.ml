open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_patt_ground ?loc:(loc = LocTest.gen ()) _ =
  Syntax.patt_ground loc

let fresh_patt_var ?loc:(loc = LocTest.gen ()) ?id:(id = SymTest.fresh_sym ()) _ =
  Syntax.patt_var loc id

(* Location Stripping *)

let deloc_patt = function
  | Syntax.PattGround _ -> Syntax.patt_ground LocTest.dummy
  | Syntax.PattVar patt -> Syntax.patt_var LocTest.dummy patt.id

(* Assertions *)

let patt_not_equal = TestUtils.not_equal "Patterns" Syntax.pp_patt

let assert_patt_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.PattGround expected, Syntax.PattGround actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.PattVar expected, Syntax.PattVar actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> patt_not_equal ~ctxt expected actual

(* Constructors *)

let test_patt_ground ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.patt_ground loc in
  match expected with
    | Syntax.PattGround actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> patt_not_equal ~ctxt expected actual

let test_patt_var ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Syntax.patt_var loc id in
  match expected with
    | Syntax.PattVar actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> patt_not_equal ~ctxt expected actual

let test_constructor =
  "Constructors" >::: [
    "Ground"     >:: test_patt_ground;
    "Identifier" >:: test_patt_var;
  ]

(* Locations *)

let assert_loc_patt = SyntaxUtils.assert_loc Syntax.loc_patt

let test_loc_patt_ground = assert_loc_patt (fun loc -> fresh_patt_ground ~loc ())
let test_loc_patt_var = assert_loc_patt (fun loc -> fresh_patt_var ~loc ())

let test_loc =
  "Locations" >::: [
    "Ground"     >:: test_loc_patt_ground;
    "Identifier" >:: test_loc_patt_var;
  ]

(* Pretty Printing *)

let assert_pp_patt = PrettyTest.assert_pp Syntax.pp_patt

let test_pp_patt_ground ctxt =
  fresh_patt_ground ()
    |> assert_pp_patt ~ctxt ["_"]

let test_pp_patt_var ctxt =
  let lexeme = "testId" in
  let id = SymTest.fresh_sym ~id:lexeme () in
  fresh_patt_var ~id ()
    |> assert_pp_patt ~ctxt [lexeme]

let test_pp =
  "Pretty Printing" >::: [
    "Ground"     >:: test_pp_patt_ground;
    "Identifier" >:: test_pp_patt_var;
  ]

(* Test Suite *)

let suite =
  "Patterns" >::: [
    test_constructor;
    test_loc;
    test_pp;
  ]
