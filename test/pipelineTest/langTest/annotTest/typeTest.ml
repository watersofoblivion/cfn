open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let type_not_equal = TestUtils.not_equal "Types" Annot.pp_ty

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.TyBool, Annot.TyBool
  | Annot.TyInt, Annot.TyInt
  | Annot.TyLong, Annot.TyLong
  | Annot.TyFloat, Annot.TyFloat
  | Annot.TyDouble, Annot.TyDouble
  | Annot.TyRune, Annot.TyRune
  | Annot.TyString, Annot.TyString -> ()
  | expected, actual -> type_not_equal ~ctxt expected actual

(* Constructors *)

let test_ty_bool ctxt =
  let expected = Annot.ty_bool in
  match expected with
    | Annot.TyBool -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_int ctxt =
  let expected = Annot.ty_int in
  match expected with
    | Annot.TyInt -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_long ctxt =
  let expected = Annot.ty_long in
  match expected with
    | Annot.TyLong -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_float ctxt =
  let expected = Annot.ty_float in
  match expected with
    | Annot.TyFloat -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_double ctxt =
  let expected = Annot.ty_double in
  match expected with
    | Annot.TyDouble -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_rune ctxt =
  let expected = Annot.ty_rune in
  match expected with
    | Annot.TyRune -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_string ctxt =
  let expected = Annot.ty_string in
  match expected with
    | Annot.TyString -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_constructors =
  "Constructors" >::: [
    "Boolean" >:: test_ty_bool;
    "Integer" >:: test_ty_int;
    "Long"    >:: test_ty_long;
    "Float"   >:: test_ty_float;
    "Double"  >:: test_ty_double;
    "Rune"    >:: test_ty_rune;
    "String"  >:: test_ty_string;
  ]

(* Operations *)

(* Equality *)

let types = [
  Annot.ty_bool;
  Annot.ty_int;
  Annot.ty_long;
  Annot.ty_float;
  Annot.ty_double;
  Annot.ty_rune;
  Annot.ty_string
]

let printer ty =
  fprintf str_formatter"%a" Annot.pp_ty ty
    |> flush_str_formatter

let test_ty_equal_equal ctxt =
  let assert_ty_equal ty = assert_equal ~ctxt ~printer ~cmp:Annot.ty_equal ty ty in
  List.iter assert_ty_equal types

let test_ty_equal_not_equal ctxt =
  let rec not_self acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Annot.ty_equal ty ty' |> not in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_self (hd :: acc) tl
  in
  not_self [] types

let assert_is category matcher ty =
  let msg =
    fprintf str_formatter "Expected %a to be %s" Annot.pp_ty ty category
      |> flush_str_formatter
  in
  matcher ty
    |> assert_bool msg

let assert_is_not category matcher ty =
  let category = "not " ^ category in
  let matcher ty = not (matcher ty) in
  assert_is category matcher ty

let assert_ty_is category matcher expected _ =
  let assert_is = assert_is category matcher in
  let assert_is_not = assert_is_not category matcher in
  List.iter assert_is expected;
  types
    |> List.filter (fun ty -> not (List.mem ty expected))
    |> List.iter assert_is_not

let test_ty_is_numeric = assert_ty_is "numeric" Annot.ty_is_numeric [
  Annot.ty_int;
  Annot.ty_long;
  Annot.ty_float;
  Annot.ty_double;
]

let test_ty_is_integral = assert_ty_is "integral" Annot.ty_is_integral [
  Annot.ty_int;
  Annot.ty_long;
]

let test_ty_is_floating_point = assert_ty_is "floating point" Annot.ty_is_floating_point [
  Annot.ty_float;
  Annot.ty_double;
]

let test_ty_is_logical = assert_ty_is "logical" Annot.ty_is_logical [
  Annot.ty_bool;
]

let test_operations =
  "Operations" >::: [
    "Equality" >::: [
      "Equal"     >:: test_ty_equal_equal;
      "Not Equal" >:: test_ty_equal_not_equal;
    ];
    "Categorization" >::: [
      "Numeric"        >:: test_ty_is_numeric;
      "Integral"       >:: test_ty_is_integral;
      "Floating Point" >:: test_ty_is_floating_point;
      "Logical"        >:: test_ty_is_logical;
    ];
  ]

(* Pretty Printing *)

let assert_pp_ty = PrettyTest.assert_pp Annot.pp_ty

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

let test_pp =
  "Pretty Printing" >::: [
    "Boolean" >:: test_pp_ty_bool;
    "Integer" >:: test_pp_ty_int;
    "Long"    >:: test_pp_ty_long;
    "Float"   >:: test_pp_ty_float;
    "Double"  >:: test_pp_ty_double;
    "Rune"    >:: test_pp_ty_rune;
    "String"  >:: test_pp_ty_string;
  ]

(* Test Suite *)

let suite =
  "Types" >::: [
    test_constructors;
    test_operations;
    test_pp;
  ]
