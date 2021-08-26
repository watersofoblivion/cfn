open Format

open OUnit2

open Mono

open CommonTest

(* Assertions *)

let type_not_equal = TestUtils.not_equal "Types" Fmt.ty

let assert_ty_equal ~ctxt expected actual = match (expected, actual) with
  | Type.Bool, Type.Bool
  | Type.Int, Type.Int
  | Type.Long, Type.Long
  | Type.Float, Type.Float
  | Type.Double, Type.Double
  | Type.Rune, Type.Rune
  | Type.String, Type.String -> ()
  | expected, actual -> type_not_equal ~ctxt expected actual

(* Constructors *)

let test_bool ctxt =
  let expected = Type.bool in
  match expected with
    | Type.Bool -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_int ctxt =
  let expected = Type.int in
  match expected with
    | Type.Int -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_long ctxt =
  let expected = Type.long in
  match expected with
    | Type.Long -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_float ctxt =
  let expected = Type.float in
  match expected with
    | Type.Float -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_double ctxt =
  let expected = Type.double in
  match expected with
    | Type.Double -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_rune ctxt =
  let expected = Type.rune in
  match expected with
    | Type.Rune -> ()
    | actual -> type_not_equal ~ctxt expected actual

let test_string ctxt =
  let expected = Type.string in
  match expected with
    | Type.String -> ()
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
    Type.bool;
    Type.int; Type.long;
    Type.float; Type.double;
    Type.rune; Type.string;
  ]

let test_equal_not_equal ctxt =
  let rec not_this acc = function
    | [] -> ()
    | hd :: tl ->
      let assert_not_equal ty =
        let cmp ty ty' = Type.equal ty ty' |> not in
        let printer ty =
          fprintf str_formatter"%a" Fmt.ty ty
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Types are equal" hd ty
      in
      List.iter assert_not_equal acc;
      List.iter assert_not_equal tl;
      not_this (hd :: acc) tl
  in
  not_this [] [
    Type.bool;
    Type.int; Type.long;
    Type.float; Type.double;
    Type.rune; Type.string;
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
