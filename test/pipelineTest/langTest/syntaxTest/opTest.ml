open OUnit2

open CommonTest

(* Fixtures *)

let fresh_un _ =
  ()
    |> LocTest.gen
    |> Syntax.un_neg

let fresh_bin _ =
  ()
    |> LocTest.gen
    |> Syntax.bin_add

(* Utilities *)

let deloc_un = function
  | Syntax.UnNeg _ -> Syntax.un_neg LocTest.dummy

let deloc_bin = function
  | Syntax.BinAdd _ -> Syntax.bin_add LocTest.dummy
  | Syntax.BinSub _ -> Syntax.bin_sub LocTest.dummy
  | Syntax.BinMul _ -> Syntax.bin_mul LocTest.dummy
  | Syntax.BinDiv _ -> Syntax.bin_div LocTest.dummy
  | Syntax.BinMod _ -> Syntax.bin_mod LocTest.dummy
  | Syntax.BinExp _ -> Syntax.bin_exp LocTest.dummy

(* Assertions *)

(* let un_not_equal = TestUtils.not_equal "Unary operators" Syntax.pp_un *)
let bin_not_equal = TestUtils.not_equal "Binary operators" Syntax.pp_bin

let assert_un_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.UnNeg expected, Syntax.UnNeg actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc

let assert_bin_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.BinAdd expected, Syntax.BinAdd actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinSub expected, Syntax.BinSub actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinMul expected, Syntax.BinMul actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinDiv expected, Syntax.BinDiv actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinMod expected, Syntax.BinMod actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinExp expected, Syntax.BinExp actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | expected, actual -> bin_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_un_neg ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.un_neg loc in
  match expected with
    | Syntax.UnNeg actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc

let test_bin_add ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_add loc in
  match expected with
    | Syntax.BinAdd actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_sub ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_sub loc in
  match expected with
    | Syntax.BinSub actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_mul ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_mul loc in
  match expected with
    | Syntax.BinMul actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_div ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_div loc in
  match expected with
    | Syntax.BinDiv actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_mod ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_mod loc in
  match expected with
    | Syntax.BinMod actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_exp ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_exp loc in
  match expected with
    | Syntax.BinExp actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

(* Test Suite *)

let suite =
  "Operators" >::: [
    "Constructors" >::: [
      "Unary" >::: [
        "Negation" >:: test_un_neg;
      ];
      "Binary" >::: [
        "Addition"       >:: test_bin_add;
        "Subtraction"    >:: test_bin_sub;
        "Multiplication" >:: test_bin_mul;
        "Division"       >:: test_bin_div;
        "Modulus"        >:: test_bin_mod;
        "Exponentiation" >:: test_bin_exp;
      ];
    ];
  ]
