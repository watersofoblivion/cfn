(* Types *)

open OUnit2

open CommonTest

(* Assertions *)

let assert_convert_ty mono clos ctxt =
  let env = EnvTest.fresh () in
  ClosTest.assert_ty_equal ~ctxt clos
    |> Conv.convert_ty env mono

(* Tests *)

let test_convert_ty_bool = assert_convert_ty Mono.ty_bool Clos.ty_bool
let test_convert_ty_int = assert_convert_ty Mono.ty_int Clos.ty_int
let test_convert_ty_long = assert_convert_ty Mono.ty_long Clos.ty_long
let test_convert_ty_float = assert_convert_ty Mono.ty_float Clos.ty_float
let test_convert_ty_double = assert_convert_ty Mono.ty_double Clos.ty_double
let test_convert_ty_rune = assert_convert_ty Mono.ty_rune Clos.ty_rune
let test_convert_ty_string = assert_convert_ty Mono.ty_string Clos.ty_string

(* Test Suite *)

let suite =
  "Types" >::: [
    "Boolean" >:: test_convert_ty_bool;
    "Integer" >:: test_convert_ty_int;
    "Long"    >:: test_convert_ty_long;
    "Float"   >:: test_convert_ty_float;
    "Double"  >:: test_convert_ty_double;
    "Rune"    >:: test_convert_ty_rune;
    "String"  >:: test_convert_ty_string;
  ]
