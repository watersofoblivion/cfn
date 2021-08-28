open Format

open OUnit2

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

(* Equality *)

let test_equal_equal ctxt =
  List.iter (fun ty -> assert_ty_equal ~ctxt ty ty) [
    Mono.ty_bool;
    Mono.ty_int; Mono.ty_long;
    Mono.ty_float; Mono.ty_double;
    Mono.ty_rune; Mono.ty_string;
  ]

let test_equal_not_equal ctxt =
  let rec not_this acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Mono.ty_equal ty ty' |> not in
        let printer ty =
          fprintf str_formatter"%a" Mono.pp_ty ty
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_this (hd :: acc) tl
  in
  not_this [] [
    Mono.ty_bool;
    Mono.ty_int; Mono.ty_long;
    Mono.ty_float; Mono.ty_double;
    Mono.ty_rune; Mono.ty_string;
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
