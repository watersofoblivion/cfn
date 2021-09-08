open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let type_not_equal = TestUtils.not_equal "Types" Mono.pp_ty

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.TyBool, Mono.TyBool
  | Mono.TyInt, Mono.TyInt
  | Mono.TyLong, Mono.TyLong
  | Mono.TyFloat, Mono.TyFloat
  | Mono.TyDouble, Mono.TyDouble
  | Mono.TyRune, Mono.TyRune
  | Mono.TyString, Mono.TyString -> ()
  | expected, actual -> type_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_bool ctxt =
  let expected = Mono.ty_bool in
  match expected with
    | Mono.TyBool -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_int ctxt =
  let expected = Mono.ty_int in
  match expected with
    | Mono.TyInt -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_long ctxt =
  let expected = Mono.ty_long in
  match expected with
    | Mono.TyLong -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_float ctxt =
  let expected = Mono.ty_float in
  match expected with
    | Mono.TyFloat -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_double ctxt =
  let expected = Mono.ty_double in
  match expected with
    | Mono.TyDouble -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_rune ctxt =
  let expected = Mono.ty_rune in
  match expected with
    | Mono.TyRune -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_string ctxt =
  let expected = Mono.ty_string in
  match expected with
    | Mono.TyString -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_constructors =
  "Constructors" >::: [
    "Boolean" >:: test_bool;
    "Integer" >:: test_int;
    "Long"    >:: test_long;
    "Float"   >:: test_float;
    "Double"  >:: test_double;
    "Rune"    >:: test_rune;
    "String"  >:: test_string;
  ]

(* Operations *)

let types = [
  Mono.ty_bool;
  Mono.ty_int;
  Mono.ty_long;
  Mono.ty_float;
  Mono.ty_double;
  Mono.ty_rune;
  Mono.ty_string
]

let printer ty =
  fprintf str_formatter"%a" Mono.pp_ty ty
    |> flush_str_formatter

let test_ty_equal_equal ctxt =
  let assert_ty_equal ty = assert_equal ~ctxt ~printer ~cmp:Mono.ty_equal ty ty in
  List.iter assert_ty_equal types

let test_ty_equal_not_equal ctxt =
  let rec not_self acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Mono.ty_equal ty ty' |> not in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_self (hd :: acc) tl
  in
  not_self [] types

let assert_is category matcher ty =
  let msg =
    fprintf str_formatter "Expected %a to be %s" Mono.pp_ty ty category
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

let test_ty_is_numeric = assert_ty_is "numeric" Mono.ty_is_numeric [
  Mono.ty_int;
  Mono.ty_long;
  Mono.ty_float;
  Mono.ty_double;
]

let test_ty_is_integral = assert_ty_is "integral" Mono.ty_is_integral [
  Mono.ty_int;
  Mono.ty_long;
]

let test_ty_is_floating_point = assert_ty_is "floating point" Mono.ty_is_floating_point [
  Mono.ty_float;
  Mono.ty_double;
]

let test_ty_is_logical = assert_ty_is "logical" Mono.ty_is_logical [
  Mono.ty_bool;
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

let assert_pp_ty = PrettyTest.assert_pp Mono.pp_ty

let test_pp_ty_bool ctxt =
  Mono.ty_bool
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_pp_ty_int ctxt =
  Mono.ty_int
    |> assert_pp_ty ~ctxt [Prim.id_int]

let test_pp_ty_long ctxt =
  Mono.ty_long
    |> assert_pp_ty ~ctxt [Prim.id_long]

let test_pp_ty_float ctxt =
  Mono.ty_float
    |> assert_pp_ty ~ctxt [Prim.id_float]

let test_pp_ty_double ctxt =
  Mono.ty_double
    |> assert_pp_ty ~ctxt [Prim.id_double]

let test_pp_ty_rune ctxt =
  Mono.ty_rune
    |> assert_pp_ty ~ctxt [Prim.id_rune]

let test_pp_ty_string ctxt =
  Mono.ty_string
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
  ]
