(* Types *)

open OUnit2

open CommonTest

(* Assertions *)

let assert_mono_ty ir mono ctxt =
  let env = EnvTest.fresh () in
  MonoTest.assert_ty_equal ~ctxt mono
    |> Monomorph.mono_ty env ir

(* Tests *)

let test_mono_ty_bool = assert_mono_ty Ir.ty_bool Mono.ty_bool
let test_mono_ty_int = assert_mono_ty Ir.ty_int Mono.ty_int
let test_mono_ty_long = assert_mono_ty Ir.ty_long Mono.ty_long
let test_mono_ty_float = assert_mono_ty Ir.ty_float Mono.ty_float
let test_mono_ty_double = assert_mono_ty Ir.ty_double Mono.ty_double
let test_mono_ty_rune = assert_mono_ty Ir.ty_rune Mono.ty_rune
let test_mono_ty_string = assert_mono_ty Ir.ty_string Mono.ty_string

(* Test Suite *)

let suite =
  "Types" >::: [
    "Boolean" >:: test_mono_ty_bool;
    "Integer" >:: test_mono_ty_int;
    "Long"    >:: test_mono_ty_long;
    "Float"   >:: test_mono_ty_float;
    "Double"  >:: test_mono_ty_double;
    "Rune"    >:: test_mono_ty_rune;
    "String"  >:: test_mono_ty_string;
  ]
