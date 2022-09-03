(* Builtins *)

open OUnit2

open CommonTest

(* Assertions *)

let integral_tys = [
  (Mono.ty_int, Clos.ty_int);
  (Mono.ty_long, Clos.ty_long);
]
let floating_point_tys = [
  (Mono.ty_float, Clos.ty_float);
  (Mono.ty_double, Clos.ty_double);
]
let numeric_tys = integral_tys @ floating_point_tys
let string_tys = [
  (Mono.ty_rune, Clos.ty_rune);
  (Mono.ty_string, Clos.ty_string);
]
let all_tys = [
  (Mono.ty_bool, Clos.ty_bool)
] @ numeric_tys @ string_tys

let assert_convert_builtin ~ctxt mono clos ty =
  let env = EnvTest.fresh () in
  Conv.convert_builtin env mono (fun arity builtin ->
    ClosTest.assert_arity_equal ~ctxt ty arity;
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let assert_convert_builtin_eq ~ctxt mono clos =
  let assert_convert_builtin_eq (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Clos.arity_fixed [clos_ty; clos_ty] Clos.ty_bool
      |> assert_convert_builtin ~ctxt mono clos
  in
  List.iter assert_convert_builtin_eq all_tys

let assert_convert_builtin_cmp ~ctxt mono clos =
  let assert_convert_builtin_cmp (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Clos.arity_fixed [clos_ty; clos_ty] Clos.ty_bool
      |> assert_convert_builtin ~ctxt mono clos
  in
  List.iter assert_convert_builtin_cmp numeric_tys

let assert_convert_builtin_bin_op ~ctxt tys mono clos =
  let assert_convert_builtin_bin_op (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Clos.arity_fixed [clos_ty; clos_ty] clos_ty
      |> assert_convert_builtin ~ctxt mono clos
  in
  List.iter assert_convert_builtin_bin_op tys

let assert_convert_builtin_bin_op_numeric = assert_convert_builtin_bin_op numeric_tys
let assert_convert_builtin_bin_op_integral = assert_convert_builtin_bin_op integral_tys
let assert_convert_builtin_bin_op_floating_point = assert_convert_builtin_bin_op floating_point_tys

let assert_convert_builtin_un_op ~ctxt tys mono clos =
  let assert_convert_builtin_un_op (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Clos.arity_fixed [clos_ty] clos_ty
      |> assert_convert_builtin ~ctxt mono clos
  in
  List.iter assert_convert_builtin_un_op tys

let assert_convert_builtin_un_op_numeric = assert_convert_builtin_un_op numeric_tys
let assert_convert_builtin_un_op_integral = assert_convert_builtin_un_op integral_tys

(* Tests *)

let test_convert_builtin_struct_eq ctxt = assert_convert_builtin_eq ~ctxt Mono.builtin_struct_eq Clos.builtin_struct_eq
let test_convert_builtin_struct_neq ctxt = assert_convert_builtin_eq ~ctxt Mono.builtin_struct_neq Clos.builtin_struct_neq
let test_convert_builtin_phys_eq ctxt = assert_convert_builtin_eq ~ctxt Mono.builtin_phys_eq Clos.builtin_phys_eq
let test_convert_builtin_phys_neq ctxt = assert_convert_builtin_eq ~ctxt Mono.builtin_phys_neq Clos.builtin_phys_neq

let test_convert_builtin_lt ctxt = assert_convert_builtin_cmp ~ctxt Mono.builtin_lt Clos.builtin_lt
let test_convert_builtin_lte ctxt = assert_convert_builtin_cmp ~ctxt Mono.builtin_lte Clos.builtin_lte
let test_convert_builtin_gt ctxt = assert_convert_builtin_cmp ~ctxt Mono.builtin_gt Clos.builtin_gt
let test_convert_builtin_gte ctxt = assert_convert_builtin_cmp ~ctxt Mono.builtin_gte Clos.builtin_gte

let test_convert_builtin_lsl ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_lsl Clos.builtin_lsl
let test_convert_builtin_lsr ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_lsr Clos.builtin_lsr
let test_convert_builtin_asl ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_asl Clos.builtin_asl
let test_convert_builtin_asr ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_asr Clos.builtin_asr

let test_convert_builtin_neg ctxt = assert_convert_builtin_un_op_numeric ~ctxt Mono.builtin_neg Clos.builtin_neg
let test_convert_builtin_add ctxt = assert_convert_builtin_bin_op_numeric ~ctxt Mono.builtin_add Clos.builtin_add
let test_convert_builtin_sub ctxt = assert_convert_builtin_bin_op_numeric ~ctxt Mono.builtin_sub Clos.builtin_sub
let test_convert_builtin_mul ctxt = assert_convert_builtin_bin_op_numeric ~ctxt Mono.builtin_mul Clos.builtin_mul
let test_convert_builtin_div ctxt = assert_convert_builtin_bin_op_numeric ~ctxt Mono.builtin_div Clos.builtin_div
let test_convert_builtin_mod ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_mod Clos.builtin_mod
let test_convert_builtin_exp ctxt = assert_convert_builtin_bin_op_floating_point ~ctxt Mono.builtin_exp Clos.builtin_exp

let test_convert_builtin_bit_and ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_bit_and Clos.builtin_bit_and
let test_convert_builtin_bit_or ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_bit_or Clos.builtin_bit_or
let test_convert_builtin_bit_not ctxt = assert_convert_builtin_un_op_integral ~ctxt Mono.builtin_bit_not Clos.builtin_bit_not
let test_convert_builtin_bit_xor ctxt = assert_convert_builtin_bin_op_integral ~ctxt Mono.builtin_bit_xor Clos.builtin_bit_xor

let test_convert_builtin_log_not ctxt =
  Clos.arity_fixed [Clos.ty_bool] Clos.ty_bool
    |> assert_convert_builtin ~ctxt Mono.builtin_log_not Clos.builtin_log_not

let test_convert_builtin_promote ctxt =
  let assert_convert_builtin_promote (mono_sub, mono_sup, clos_sub, clos_sup) =
    let mono = Mono.builtin_promote mono_sub mono_sup in
    let clos = Clos.builtin_promote clos_sub clos_sup in
    Clos.arity_fixed [clos_sub] clos_sup
      |> assert_convert_builtin ~ctxt mono clos
  in
  List.iter assert_convert_builtin_promote [
    (Mono.ty_int, Mono.ty_long, Clos.ty_int, Clos.ty_long);
    (Mono.ty_int, Mono.ty_double, Clos.ty_int, Clos.ty_double);
    (Mono.ty_float, Mono.ty_double, Clos.ty_float, Clos.ty_double);
  ]

let test_convert_builtin_concat ctxt =
  let assert_convert_builtin_concat (mono_ty, clos_ty) =
    let mono = Mono.builtin_concat mono_ty in
    let clos = Clos.builtin_concat clos_ty in
    Clos.arity_var clos_ty clos_ty
      |> assert_convert_builtin ~ctxt mono clos
  in
  List.iter assert_convert_builtin_concat [
    (Mono.ty_string, Clos.ty_string);
  ]

(* Test Suite *)

let suite =
  "Built-in Functions" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_convert_builtin_struct_eq;
          "Not Equal" >:: test_convert_builtin_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_convert_builtin_phys_eq;
          "Not Equal" >:: test_convert_builtin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_convert_builtin_lt;
      "Less Than or Equal"    >:: test_convert_builtin_lte;
      "Greater Than"          >:: test_convert_builtin_gt;
      "Greater Than or Equal" >:: test_convert_builtin_gte;
    ];
    "Shift" >::: [
      "Logical" >::: [
        "Left"  >:: test_convert_builtin_lsl;
        "Right" >:: test_convert_builtin_lsr;
      ];
      "Arithmetic" >::: [
        "Left"  >:: test_convert_builtin_asl;
        "Right" >:: test_convert_builtin_asr;
      ];
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_convert_builtin_neg;
      "Addition"       >:: test_convert_builtin_add;
      "Subtraction"    >:: test_convert_builtin_sub;
      "Multiplication" >:: test_convert_builtin_mul;
      "Division"       >:: test_convert_builtin_div;
      "Modulus"        >:: test_convert_builtin_mod;
      "Exponentiation" >:: test_convert_builtin_exp;
    ];
    "Bitwise Operations" >::: [
      "AND" >:: test_convert_builtin_bit_and;
      "OR"  >:: test_convert_builtin_bit_or;
      "NOT" >:: test_convert_builtin_bit_not;
      "XOR" >:: test_convert_builtin_bit_xor;
    ];
    "Logic" >::: [
      "NOT" >:: test_convert_builtin_log_not;
    ];
    "Type Promotion" >:: test_convert_builtin_promote;
    "Concatenation"  >:: test_convert_builtin_concat;
  ]
