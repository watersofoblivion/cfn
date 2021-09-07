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

(* Equality *)

let test_equal_equal ctxt =
  List.iter (fun ty -> assert_ty_equal ~ctxt ty ty) [
    Ir.ty_bool;
    Ir.ty_int; Ir.ty_long;
    Ir.ty_float; Ir.ty_double;
    Ir.ty_rune; Ir.ty_string;
  ]

let test_equal_not_equal ctxt =
  let rec not_this acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Ir.ty_equal ty ty' |> not in
        let printer ty =
          fprintf str_formatter "%a" Ir.pp_ty ty
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_this (hd :: acc) tl
  in
  not_this [] [
    Ir.ty_bool;
    Ir.ty_int; Ir.ty_long;
    Ir.ty_float; Ir.ty_double;
    Ir.ty_rune; Ir.ty_string;
  ]

let test_operations =
  "Operations" >::: [
    "Equality" >::: [
      "Equal"     >:: test_equal_equal;
      "Not Equal" >:: test_equal_not_equal;
    ]
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
