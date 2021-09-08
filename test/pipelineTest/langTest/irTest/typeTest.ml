open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let type_not_equal = TestUtils.not_equal "Types" Ir.pp_ty

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.TyBool, Ir.TyBool
  | Ir.TyInt, Ir.TyInt
  | Ir.TyLong, Ir.TyLong
  | Ir.TyFloat, Ir.TyFloat
  | Ir.TyDouble, Ir.TyDouble
  | Ir.TyRune, Ir.TyRune
  | Ir.TyString, Ir.TyString -> ()
  | expected, actual -> type_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_bool ctxt =
  let expected = Ir.ty_bool in
  match expected with
    | Ir.TyBool -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_int ctxt =
  let expected = Ir.ty_int in
  match expected with
    | Ir.TyInt -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_long ctxt =
  let expected = Ir.ty_long in
  match expected with
    | Ir.TyLong -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_float ctxt =
  let expected = Ir.ty_float in
  match expected with
    | Ir.TyFloat -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_double ctxt =
  let expected = Ir.ty_double in
  match expected with
    | Ir.TyDouble -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_rune ctxt =
  let expected = Ir.ty_rune in
  match expected with
    | Ir.TyRune -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_string ctxt =
  let expected = Ir.ty_string in
  match expected with
    | Ir.TyString -> ()
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
  Ir.ty_bool;
  Ir.ty_int;
  Ir.ty_long;
  Ir.ty_float;
  Ir.ty_double;
  Ir.ty_rune;
  Ir.ty_string
]

let printer ty =
  fprintf str_formatter"%a" Ir.pp_ty ty
    |> flush_str_formatter

let test_ty_equal_equal ctxt =
  let assert_ty_equal ty = assert_equal ~ctxt ~printer ~cmp:Ir.ty_equal ty ty in
  List.iter assert_ty_equal types

let test_ty_equal_not_equal ctxt =
  let rec not_self acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Ir.ty_equal ty ty' |> not in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_self (hd :: acc) tl
  in
  not_self [] types

let assert_is category matcher ty =
  let msg =
    fprintf str_formatter "Expected %a to be %s" Ir.pp_ty ty category
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

let test_ty_is_numeric = assert_ty_is "numeric" Ir.ty_is_numeric [
  Ir.ty_int;
  Ir.ty_long;
  Ir.ty_float;
  Ir.ty_double;
]

let test_ty_is_integral = assert_ty_is "integral" Ir.ty_is_integral [
  Ir.ty_int;
  Ir.ty_long;
]

let test_ty_is_floating_point = assert_ty_is "floating point" Ir.ty_is_floating_point [
  Ir.ty_float;
  Ir.ty_double;
]

let test_ty_is_logical = assert_ty_is "logical" Ir.ty_is_logical [
  Ir.ty_bool;
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

let assert_pp_ty = PrettyTest.assert_pp Ir.pp_ty

let test_pp_ty_bool ctxt =
  Ir.ty_bool
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_pp_ty_int ctxt =
  Ir.ty_int
    |> assert_pp_ty ~ctxt [Prim.id_int]

let test_pp_ty_long ctxt =
  Ir.ty_long
    |> assert_pp_ty ~ctxt [Prim.id_long]

let test_pp_ty_float ctxt =
  Ir.ty_float
    |> assert_pp_ty ~ctxt [Prim.id_float]

let test_pp_ty_double ctxt =
  Ir.ty_double
    |> assert_pp_ty ~ctxt [Prim.id_double]

let test_pp_ty_rune ctxt =
  Ir.ty_rune
    |> assert_pp_ty ~ctxt [Prim.id_rune]

let test_pp_ty_string ctxt =
  Ir.ty_string
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
