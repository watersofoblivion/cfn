(* Built-in Functions *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_builtin ?ty:(ty = Mono.ty_bool) _ =
  Mono.builtin_struct_eq ty

(* Assertions *)

let arity_not_equal = TestUtils.not_equal "Arities" Mono.pp_arity
let builtin_not_equal = TestUtils.not_equal "Built-in functions" Mono.pp_builtin

let assert_arity_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.ArityFixed expected, Mono.ArityFixed actual ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Arities are not equal" expected.arity actual.arity;
    List.iter2 (TypeTest.assert_ty_equal ~ctxt) expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | Mono.ArityVar expected, Mono.ArityVar actual ->
    TypeTest.assert_ty_equal ~ctxt expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | expected, actual -> arity_not_equal ~ctxt expected actual

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.BuiltinStructEq expected, Mono.BuiltinStructEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinStructNeq expected, Mono.BuiltinStructNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinPhysEq expected, Mono.BuiltinPhysEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinPhysNeq expected, Mono.BuiltinPhysNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinLt expected, Mono.BuiltinLt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinLte expected, Mono.BuiltinLte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinGt expected, Mono.BuiltinGt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinGte expected, Mono.BuiltinGte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinAdd expected, Mono.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinSub expected, Mono.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinMul expected, Mono.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinDiv expected, Mono.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinMod expected, Mono.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinExp expected, Mono.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinNeg expected, Mono.BuiltinNeg actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinBitAnd expected, Mono.BuiltinBitAnd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinBitOr expected, Mono.BuiltinBitOr actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinBitNot expected, Mono.BuiltinBitNot actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinBitXor expected, Mono.BuiltinBitXor actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Mono.BuiltinLogNot, Mono.BuiltinLogNot -> ()
  | Mono.BuiltinPromote expected, Mono.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Mono.BuiltinConcat expected, Mono.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_arity_fixed ctxt =
  let args = [Mono.ty_bool; Mono.ty_int] in
  let res = Mono.ty_float in
  let expected = Mono.arity_fixed args res in
  match expected with
    | Mono.ArityFixed actual ->
      assert_equal ~ctxt 2 actual.arity;
      List.iter2 (TypeTest.assert_ty_equal ~ctxt) args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_arity_var ctxt =
  let args = Mono.ty_bool in
  let res = Mono.ty_float in
  let expected = Mono.arity_var args res in
  match expected with
    | Mono.ArityVar actual ->
      TypeTest.assert_ty_equal ~ctxt args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_builtin_struct_eq ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_struct_eq ty in
  match expected with
    | BuiltinStructEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_struct_neq ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_struct_neq ty in
  match expected with
    | BuiltinStructNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_eq ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_phys_eq ty in
  match expected with
    | BuiltinPhysEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_neq ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_phys_neq ty in
  match expected with
    | BuiltinPhysNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lt ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_lt ty in
  match expected with
    | Mono.BuiltinLt actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lte ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_lte ty in
  match expected with
    | Mono.BuiltinLte actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_gt ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_gt ty in
  match expected with
    | Mono.BuiltinGt actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_gte ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_gte ty in
  match expected with
    | Mono.BuiltinGte actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_add ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_add ty in
  match expected with
    | Mono.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_sub ty in
  match expected with
    | Mono.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_mul ty in
  match expected with
    | Mono.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_div ty in
  match expected with
    | Mono.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Mono.ty_int in
  let expected = Mono.builtin_mod ty in
  match expected with
    | Mono.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Mono.ty_float in
  let expected = Mono.builtin_exp ty in
  match expected with
    | Mono.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_neg ctxt =
  let ty = Mono.ty_float in
  let expected = Mono.builtin_neg ty in
  match expected with
    | Mono.BuiltinNeg actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_and ctxt =
  let ty = Mono.ty_float in
  let expected = Mono.builtin_bit_and ty in
  match expected with
    | Mono.BuiltinBitAnd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_or ctxt =
  let ty = Mono.ty_float in
  let expected = Mono.builtin_bit_or ty in
  match expected with
    | Mono.BuiltinBitOr actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_not ctxt =
  let ty = Mono.ty_float in
  let expected = Mono.builtin_bit_not ty in
  match expected with
    | Mono.BuiltinBitNot actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_xor ctxt =
  let ty = Mono.ty_float in
  let expected = Mono.builtin_bit_xor ty in
  match expected with
    | Mono.BuiltinBitXor actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_log_not ctxt =
  let expected = Mono.builtin_log_not in
  match expected with
    | Mono.BuiltinLogNot -> ()
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Mono.ty_int in
  let sup = Mono.ty_long in
  let expected = Mono.builtin_promote sub sup in
  match expected with
    | Mono.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Mono.ty_string in
  let expected = Mono.builtin_concat ty in
  match expected with
    | Mono.BuiltinConcat actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_constructors =
  "Constructors" >::: [
    "Arities" >::: [
      "Fixed"    >:: test_arity_fixed;
      "Variable" >:: test_arity_var;
    ];
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_builtin_struct_eq;
          "Not Equal" >:: test_builtin_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_builtin_phys_eq;
          "Not Equal" >:: test_builtin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_builtin_lt;
      "Less Than or Equal"    >:: test_builtin_lte;
      "Greater Than"          >:: test_builtin_gt;
      "Greater Than or Equal" >:: test_builtin_gte;
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_builtin_neg;
      "Addition"       >:: test_builtin_add;
      "Subtraction"    >:: test_builtin_sub;
      "Multiplication" >:: test_builtin_mul;
      "Division"       >:: test_builtin_div;
      "Modulus"        >:: test_builtin_mod;
      "Exponentiation" >:: test_builtin_exp;
    ];
    "Bitwise Operations" >::: [
      "AND" >:: test_builtin_bit_and;
      "OR"  >:: test_builtin_bit_or;
      "NOT" >:: test_builtin_bit_not;
      "XOR" >:: test_builtin_bit_xor;
    ];
    "Logic" >::: [
      "NOT" >:: test_builtin_log_not;
    ];
    "Type Promotion" >:: test_builtin_promote;
    "Concatenation"  >:: test_builtin_concat;
  ]

(* Pretty Printing *)

let assert_pp_arity = PrettyTest.assert_pp Mono.pp_arity
let assert_pp_builtin = PrettyTest.assert_pp Mono.pp_builtin

let test_pp_arity_fixed ctxt =
  Mono.arity_fixed [Mono.ty_bool; Mono.ty_int] Mono.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s, %s): %s" Prim.id_bool Prim.id_int Prim.id_float
       ]

let test_pp_arity_var ctxt =
  Mono.arity_var Mono.ty_bool Mono.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s...): %s" Prim.id_bool Prim.id_float
       ]

let test_pp_struct_eq ctxt =
  Mono.builtin_struct_eq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structEq[%s]" Prim.id_bool
       ]

let test_pp_struct_neq ctxt =
  Mono.builtin_struct_eq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structNeq[%s]" Prim.id_bool
       ]

let test_pp_phys_eq ctxt =
  Mono.builtin_phys_eq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physEq[%s]" Prim.id_bool
       ]

let test_pp_phys_neq ctxt =
  Mono.builtin_phys_eq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physNeq[%s]" Prim.id_bool
       ]

let test_pp_lt ctxt =
  Mono.builtin_lt Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lt[%s]" Prim.id_int
       ]

let test_pp_lte ctxt =
  Mono.builtin_lte Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lte[%s]" Prim.id_int
       ]

let test_pp_gt ctxt =
  Mono.builtin_gt Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gt[%s]" Prim.id_int
       ]

let test_pp_gte ctxt =
  Mono.builtin_gte Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gte[%s]" Prim.id_int
       ]

let test_pp_add ctxt =
  Mono.builtin_add Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "add[%s]" Prim.id_int
       ]

let test_pp_sub ctxt =
  Mono.builtin_sub Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "sub[%s]" Prim.id_int
       ]

let test_pp_mul ctxt =
  Mono.builtin_mul Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mul[%s]" Prim.id_int
       ]

let test_pp_div ctxt =
  Mono.builtin_div Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "div[%s]" Prim.id_int
       ]

let test_pp_mod ctxt =
  Mono.builtin_mod Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mod[%s]" Prim.id_int
       ]

let test_pp_exp ctxt =
  Mono.builtin_exp Mono.ty_float
    |> assert_pp_builtin ~ctxt [
         sprintf "exp[%s]" Prim.id_float
       ]

let test_pp_neg ctxt =
  Mono.builtin_neg Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "neg[%s]" Prim.id_int
       ]

let test_pp_bit_and ctxt =
  Mono.builtin_bit_and Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitAnd[%s]" Prim.id_int
       ]

let test_pp_bit_or ctxt =
  Mono.builtin_bit_or Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitOr[%s]" Prim.id_int
       ]

let test_pp_bit_not ctxt =
  Mono.builtin_bit_not Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitNot[%s]" Prim.id_int
       ]

let test_pp_bit_xor ctxt =
  Mono.builtin_bit_xor Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "bitXor[%s]" Prim.id_bool
       ]

let test_pp_log_not ctxt =
  Mono.builtin_log_not
    |> assert_pp_builtin ~ctxt ["logNot"]

let test_pp_promote ctxt =
  Mono.builtin_promote Mono.ty_int Mono.ty_double
    |> assert_pp_builtin ~ctxt [
         sprintf "logNot[%s, %s]" Prim.id_int Prim.id_double
       ]

let test_pp_concat ctxt =
  Mono.builtin_concat Mono.ty_string
    |> assert_pp_builtin ~ctxt [
         sprintf "concat[%s]" Prim.id_string
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Arities" >::: [
      "Fixed"    >:: test_pp_arity_fixed;
      "Variable" >:: test_pp_arity_var;
    ];
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_pp_struct_eq;
          "Not Equal" >:: test_pp_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_pp_phys_eq;
          "Not Equal" >:: test_pp_phys_neq;
        ];
      ];
      "Less Than"             >:: test_pp_lt;
      "Less Than or Equal"    >:: test_pp_lte;
      "Greater Than"          >:: test_pp_gt;
      "Greater Than or Equal" >:: test_pp_gte;
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_pp_neg;
      "Addition"       >:: test_pp_add;
      "Subtraction"    >:: test_pp_sub;
      "Multiplication" >:: test_pp_mul;
      "Division"       >:: test_pp_div;
      "Modulus"        >:: test_pp_mod;
      "Exponentiation" >:: test_pp_exp;
    ];
    "Bitwise Operations" >::: [
      "AND" >:: test_pp_bit_and;
      "OR"  >:: test_pp_bit_or;
      "NOT" >:: test_pp_bit_not;
      "XOR" >:: test_pp_bit_xor;
    ];
    "Logic" >::: [
      "NOT" >:: test_pp_log_not;
    ];
    "Type Promotion" >:: test_pp_promote;
    "Concatenation"  >:: test_pp_concat;
  ]

(* Type Checking *)

let test_check_struct_eq ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_struct_eq ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_struct_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_struct_neq ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_phys_eq ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_phys_eq ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_phys_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_phys_neq ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_lt ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_lt ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_lte ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_lte ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_gt ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_gt ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_gte ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_gte ty in
  Mono.arity_fixed [ty; ty] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_add ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_add ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_sub ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_sub ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_mul ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_mul ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_div ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_div ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_mod ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_mod ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_exp ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_float in
  let builtin = Mono.builtin_exp ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_neg ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_neg ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_bit_and ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_bit_and ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_bit_or ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_bit_or ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_bit_not ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_bit_not ty in
  Mono.arity_fixed [ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_bit_xor ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_int in
  let builtin = Mono.builtin_bit_xor ty in
  Mono.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_log_not ctxt =
  let env = EnvTest.fresh () in
  let builtin = Mono.builtin_log_not in
  Mono.arity_fixed [Mono.ty_bool] Mono.ty_bool
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_promote ctxt =
  let env = EnvTest.fresh () in
  let sub = Mono.ty_int in
  let sup = Mono.ty_double in
  let builtin = Mono.builtin_promote sub sup in
  Mono.arity_fixed [sub] sup
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check_concat ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_string in
  let builtin = Mono.builtin_concat ty in
  Mono.arity_var ty ty
    |> assert_arity_equal ~ctxt
    |> Mono.check_builtin env builtin

let test_check =
  "Type Checking" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_check_struct_eq;
          "Not Equal" >:: test_check_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_check_phys_eq;
          "Not Equal" >:: test_check_phys_neq;
        ];
      ];
      "Less Than"             >:: test_check_lt;
      "Less Than or Equal"    >:: test_check_lte;
      "Greater Than"          >:: test_check_gt;
      "Greater Than or Equal" >:: test_check_gte;
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_check_neg;
      "Addition"       >:: test_check_add;
      "Subtraction"    >:: test_check_sub;
      "Multiplication" >:: test_check_mul;
      "Division"       >:: test_check_div;
      "Modulus"        >:: test_check_mod;
      "Exponentiation" >:: test_check_exp;
    ];
    "Bitwise Operations" >::: [
      "AND" >:: test_check_bit_and;
      "OR"  >:: test_check_bit_or;
      "NOT" >:: test_check_bit_not;
      "XOR" >:: test_check_bit_xor;
    ];
    "Logic" >::: [
      "NOT" >:: test_check_log_not;
    ];
    "Type Promotion" >:: test_check_promote;
    "Concatenation"  >:: test_check_concat;
  ]

(* Test Suite *)

let suite =
  "Built-In Functions" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
