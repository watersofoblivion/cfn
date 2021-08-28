open Format

open OUnit2

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

let test_ty_bool ctxt =
  let expected = Clos.ty_bool in
  match expected with
    | Clos.TyBool -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_int ctxt =
  let expected = Clos.ty_int in
  match expected with
    | Clos.TyInt -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_long ctxt =
  let expected = Clos.ty_long in
  match expected with
    | Clos.TyLong -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_float ctxt =
  let expected = Clos.ty_float in
  match expected with
    | Clos.TyFloat -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_double ctxt =
  let expected = Clos.ty_double in
  match expected with
    | Clos.TyDouble -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_rune ctxt =
  let expected = Clos.ty_rune in
  match expected with
    | Clos.TyRune -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_ty_string ctxt =
  let expected = Clos.ty_string in
  match expected with
    | Clos.TyString -> ()
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

(* Test Suite *)

let suite =
  "Types" >::: [
    test_constructors;
    test_operations;
  ]
