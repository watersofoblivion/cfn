(* Types *)

open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let type_not_equal = TestUtils.not_equal "Types" Clos.pp_ty

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.TyBool, Clos.TyBool
  | Clos.TyInt, Clos.TyInt
  | Clos.TyLong, Clos.TyLong
  | Clos.TyFloat, Clos.TyFloat
  | Clos.TyDouble, Clos.TyDouble
  | Clos.TyRune, Clos.TyRune
  | Clos.TyString, Clos.TyString -> ()
  | expected, actual -> type_not_equal ~ctxt expected actual

(* Constructors *)

let test_bool ctxt =
  let expected = Clos.ty_bool in
  match expected with
    | Clos.TyBool -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_int ctxt =
  let expected = Clos.ty_int in
  match expected with
    | Clos.TyInt -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_long ctxt =
  let expected = Clos.ty_long in
  match expected with
    | Clos.TyLong -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_float ctxt =
  let expected = Clos.ty_float in
  match expected with
    | Clos.TyFloat -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_double ctxt =
  let expected = Clos.ty_double in
  match expected with
    | Clos.TyDouble -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_rune ctxt =
  let expected = Clos.ty_rune in
  match expected with
    | Clos.TyRune -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_string ctxt =
  let expected = Clos.ty_string in
  match expected with
    | Clos.TyString -> ()
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

(* Equality *)

let test_equal_equal ctxt =
  List.iter (fun ty -> assert_ty_equal ~ctxt ty ty) [
    Clos.ty_bool;
    Clos.ty_int; Clos.ty_long;
    Clos.ty_float; Clos.ty_double;
    Clos.ty_rune; Clos.ty_string;
  ]

let test_equal_not_equal ctxt =
  let rec not_this acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Clos.ty_equal ty ty' |> not in
        let printer ty =
          fprintf str_formatter"%a" Clos.pp_ty ty
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_this (hd :: acc) tl
  in
  not_this [] [
    Clos.ty_bool;
    Clos.ty_int; Clos.ty_long;
    Clos.ty_float; Clos.ty_double;
    Clos.ty_rune; Clos.ty_string;
  ]

let test_operations =
  "Operations" >::: [
    "Equality" >::: [
      "Equal"     >:: test_equal_equal;
      "Not Equal" >:: test_equal_not_equal;
    ]
  ]

(* Pretty Printing *)

let assert_pp_ty = PrettyTest.assert_pp Clos.pp_ty

let test_pp_ty_bool ctxt =
  Clos.ty_bool
    |> assert_pp_ty ~ctxt [Prim.id_bool]

let test_pp_ty_int ctxt =
  Clos.ty_int
    |> assert_pp_ty ~ctxt [Prim.id_int]

let test_pp_ty_long ctxt =
  Clos.ty_long
    |> assert_pp_ty ~ctxt [Prim.id_long]

let test_pp_ty_float ctxt =
  Clos.ty_float
    |> assert_pp_ty ~ctxt [Prim.id_float]

let test_pp_ty_double ctxt =
  Clos.ty_double
    |> assert_pp_ty ~ctxt [Prim.id_double]

let test_pp_ty_rune ctxt =
  Clos.ty_rune
    |> assert_pp_ty ~ctxt [Prim.id_rune]

let test_pp_ty_string ctxt =
  Clos.ty_string
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
