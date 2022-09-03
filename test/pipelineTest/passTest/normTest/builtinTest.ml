(* Builtins *)

open OUnit2

open CommonTest

(* Assertions *)

let integral_tys = [
  (Annot.ty_int, Ir.ty_int);
  (Annot.ty_long, Ir.ty_long);
]
let floating_point_tys = [
  (Annot.ty_float, Ir.ty_float);
  (Annot.ty_double, Ir.ty_double);
]
let numeric_tys = integral_tys @ floating_point_tys
let string_tys = [
  (Annot.ty_rune, Ir.ty_rune);
  (Annot.ty_string, Ir.ty_string);
]
let all_tys = [
  (Annot.ty_bool, Ir.ty_bool)
] @ numeric_tys @ string_tys

let assert_norm_builtin ~ctxt mono clos ty =
  let env = EnvTest.fresh () in
  Norm.norm_builtin env mono (fun arity builtin ->
    IrTest.assert_arity_equal ~ctxt ty arity;
    IrTest.assert_builtin_equal ~ctxt clos builtin)

let assert_norm_builtin_eq ~ctxt mono clos =
  let assert_norm_builtin_eq (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Ir.arity_fixed [clos_ty; clos_ty] Ir.ty_bool
      |> assert_norm_builtin ~ctxt mono clos
  in
  List.iter assert_norm_builtin_eq all_tys

let assert_norm_builtin_cmp ~ctxt mono clos =
  let assert_norm_builtin_cmp (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Ir.arity_fixed [clos_ty; clos_ty] Ir.ty_bool
      |> assert_norm_builtin ~ctxt mono clos
  in
  List.iter assert_norm_builtin_cmp numeric_tys

let assert_norm_builtin_bin_op ~ctxt tys mono clos =
  let assert_norm_builtin_bin_op (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Ir.arity_fixed [clos_ty; clos_ty] clos_ty
      |> assert_norm_builtin ~ctxt mono clos
  in
  List.iter assert_norm_builtin_bin_op tys

let assert_norm_builtin_bin_op_numeric = assert_norm_builtin_bin_op numeric_tys
let assert_norm_builtin_bin_op_integral = assert_norm_builtin_bin_op integral_tys
let assert_norm_builtin_bin_op_floating_point = assert_norm_builtin_bin_op floating_point_tys

let assert_norm_builtin_un_op ~ctxt tys mono clos =
  let assert_norm_builtin_un_op (mono_ty, clos_ty) =
    let mono = mono mono_ty in
    let clos = clos clos_ty in
    Ir.arity_fixed [clos_ty] clos_ty
      |> assert_norm_builtin ~ctxt mono clos
  in
  List.iter assert_norm_builtin_un_op tys

let assert_norm_builtin_un_op_numeric = assert_norm_builtin_un_op numeric_tys
let assert_norm_builtin_un_op_integral = assert_norm_builtin_un_op integral_tys

(* Tests *)

let test_norm_builtin_struct_eq ctxt = assert_norm_builtin_eq ~ctxt Annot.builtin_struct_eq Ir.builtin_struct_eq
let test_norm_builtin_struct_neq ctxt = assert_norm_builtin_eq ~ctxt Annot.builtin_struct_neq Ir.builtin_struct_neq
let test_norm_builtin_phys_eq ctxt = assert_norm_builtin_eq ~ctxt Annot.builtin_phys_eq Ir.builtin_phys_eq
let test_norm_builtin_phys_neq ctxt = assert_norm_builtin_eq ~ctxt Annot.builtin_phys_neq Ir.builtin_phys_neq

let test_norm_builtin_lt ctxt = assert_norm_builtin_cmp ~ctxt Annot.builtin_lt Ir.builtin_lt
let test_norm_builtin_lte ctxt = assert_norm_builtin_cmp ~ctxt Annot.builtin_lte Ir.builtin_lte
let test_norm_builtin_gt ctxt = assert_norm_builtin_cmp ~ctxt Annot.builtin_gt Ir.builtin_gt
let test_norm_builtin_gte ctxt = assert_norm_builtin_cmp ~ctxt Annot.builtin_gte Ir.builtin_gte

let test_norm_builtin_lsl ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_lsl Ir.builtin_lsl
let test_norm_builtin_lsr ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_lsr Ir.builtin_lsr
let test_norm_builtin_asl ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_asl Ir.builtin_asl
let test_norm_builtin_asr ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_asr Ir.builtin_asr

let test_norm_builtin_neg ctxt = assert_norm_builtin_un_op_numeric ~ctxt Annot.builtin_neg Ir.builtin_neg
let test_norm_builtin_add ctxt = assert_norm_builtin_bin_op_numeric ~ctxt Annot.builtin_add Ir.builtin_add
let test_norm_builtin_sub ctxt = assert_norm_builtin_bin_op_numeric ~ctxt Annot.builtin_sub Ir.builtin_sub
let test_norm_builtin_mul ctxt = assert_norm_builtin_bin_op_numeric ~ctxt Annot.builtin_mul Ir.builtin_mul
let test_norm_builtin_div ctxt = assert_norm_builtin_bin_op_numeric ~ctxt Annot.builtin_div Ir.builtin_div
let test_norm_builtin_mod ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_mod Ir.builtin_mod
let test_norm_builtin_exp ctxt = assert_norm_builtin_bin_op_floating_point ~ctxt Annot.builtin_exp Ir.builtin_exp

let test_norm_builtin_bit_and ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_bit_and Ir.builtin_bit_and
let test_norm_builtin_bit_or ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_bit_or Ir.builtin_bit_or
let test_norm_builtin_bit_not ctxt = assert_norm_builtin_un_op_integral ~ctxt Annot.builtin_bit_not Ir.builtin_bit_not
let test_norm_builtin_bit_xor ctxt = assert_norm_builtin_bin_op_integral ~ctxt Annot.builtin_bit_xor Ir.builtin_bit_xor

let test_norm_builtin_log_not ctxt =
  Ir.arity_fixed [Ir.ty_bool] Ir.ty_bool
    |> assert_norm_builtin ~ctxt Annot.builtin_log_not Ir.builtin_log_not

let test_norm_builtin_promote ctxt =
  let assert_norm_builtin_promote (mono_sub, mono_sup, clos_sub, clos_sup) =
    let mono = Annot.builtin_promote mono_sub mono_sup in
    let clos = Ir.builtin_promote clos_sub clos_sup in
    Ir.arity_fixed [clos_sub] clos_sup
      |> assert_norm_builtin ~ctxt mono clos
  in
  List.iter assert_norm_builtin_promote [
    (Annot.ty_int, Annot.ty_long, Ir.ty_int, Ir.ty_long);
    (Annot.ty_int, Annot.ty_double, Ir.ty_int, Ir.ty_double);
    (Annot.ty_float, Annot.ty_double, Ir.ty_float, Ir.ty_double);
  ]

let test_norm_builtin_concat ctxt =
  let assert_norm_builtin_concat (mono_ty, clos_ty) =
    let mono = Annot.builtin_concat mono_ty in
    let clos = Ir.builtin_concat clos_ty in
    Ir.arity_var clos_ty clos_ty
      |> assert_norm_builtin ~ctxt mono clos
  in
  List.iter assert_norm_builtin_concat [
    (Annot.ty_string, Ir.ty_string);
  ]

(* Test Suite *)

let suite =
  "Built-in Functions" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_norm_builtin_struct_eq;
          "Not Equal" >:: test_norm_builtin_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_norm_builtin_phys_eq;
          "Not Equal" >:: test_norm_builtin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_norm_builtin_lt;
      "Less Than or Equal"    >:: test_norm_builtin_lte;
      "Greater Than"          >:: test_norm_builtin_gt;
      "Greater Than or Equal" >:: test_norm_builtin_gte;
    ];
    "Shift" >::: [
      "Logical" >::: [
        "Left"  >:: test_norm_builtin_lsl;
        "Right" >:: test_norm_builtin_lsr;
      ];
      "Arithmetic" >::: [
        "Left"  >:: test_norm_builtin_asl;
        "Right" >:: test_norm_builtin_asr;
      ];
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_norm_builtin_neg;
      "Addition"       >:: test_norm_builtin_add;
      "Subtraction"    >:: test_norm_builtin_sub;
      "Multiplication" >:: test_norm_builtin_mul;
      "Division"       >:: test_norm_builtin_div;
      "Modulus"        >:: test_norm_builtin_mod;
      "Exponentiation" >:: test_norm_builtin_exp;
    ];
    "Bitwise Operations" >::: [
      "AND" >:: test_norm_builtin_bit_and;
      "OR"  >:: test_norm_builtin_bit_or;
      "NOT" >:: test_norm_builtin_bit_not;
      "XOR" >:: test_norm_builtin_bit_xor;
    ];
    "Logic" >::: [
      "NOT" >:: test_norm_builtin_log_not;
    ];
    "Type Promotion" >:: test_norm_builtin_promote;
    "Concatenation"  >:: test_norm_builtin_concat;
  ]
