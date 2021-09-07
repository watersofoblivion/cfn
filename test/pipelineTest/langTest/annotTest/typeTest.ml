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

let test_ty_equal_equal ctxt =
  List.iter (fun ty -> assert_ty_equal ~ctxt ty ty) [
    Annot.ty_bool;
    Annot.ty_int; Annot.ty_long;
    Annot.ty_float; Annot.ty_double;
    Annot.ty_rune; Annot.ty_string;
  ]

let test_ty_equal_not_equal ctxt =
  let rec not_this acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Annot.ty_equal ty ty' |> not in
        let printer ty =
          fprintf str_formatter"%a" Annot.pp_ty ty
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_this (hd :: acc) tl
  in
  not_this [] [
    Annot.ty_bool;
    Annot.ty_int; Annot.ty_long;
    Annot.ty_float; Annot.ty_double;
    Annot.ty_rune; Annot.ty_string;
  ]

let test_operations =
  "Operations" >::: [
    "Equality" >::: [
      "Equal"     >:: test_ty_equal_equal;
      "Not Equal" >:: test_ty_equal_not_equal;
    ]
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
