(* Built-in Functions *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_builtin ?ty:(ty = Clos.ty_bool) _ =
  Clos.builtin_struct_eq ty

(* Assertions *)

let arity_not_equal = TestUtils.not_equal "Arities" Clos.pp_arity
let builtin_not_equal = TestUtils.not_equal "Built-in functions" Clos.pp_builtin

let assert_arity_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.ArityFixed expected, Clos.ArityFixed actual ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Arities are not equal" expected.arity actual.arity;
    List.iter2 (TypeTest.assert_ty_equal ~ctxt) expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | Clos.ArityVar expected, Clos.ArityVar actual ->
    TypeTest.assert_ty_equal ~ctxt expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | expected, actual -> arity_not_equal ~ctxt expected actual

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.BuiltinStructEq expected, Clos.BuiltinStructEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinStructNeq expected, Clos.BuiltinStructNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinPhysEq expected, Clos.BuiltinPhysEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinPhysNeq expected, Clos.BuiltinPhysNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinLt expected, Clos.BuiltinLt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinLte expected, Clos.BuiltinLte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinGt expected, Clos.BuiltinGt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinGte expected, Clos.BuiltinGte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinAdd expected, Clos.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinSub expected, Clos.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinMul expected, Clos.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinDiv expected, Clos.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinMod expected, Clos.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinExp expected, Clos.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinNeg expected, Clos.BuiltinNeg actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinBitAnd expected, Clos.BuiltinBitAnd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinBitOr expected, Clos.BuiltinBitOr actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinBitNot expected, Clos.BuiltinBitNot actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinBitXor expected, Clos.BuiltinBitXor actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Clos.BuiltinLogNot, Clos.BuiltinLogNot -> ()
  | Clos.BuiltinPromote expected, Clos.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Clos.BuiltinConcat expected, Clos.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_arity_fixed ctxt =
  let args = [Clos.ty_bool; Clos.ty_int] in
  let res = Clos.ty_float in
  let expected = Clos.arity_fixed args res in
  match expected with
    | Clos.ArityFixed actual ->
      assert_equal ~ctxt 2 actual.arity;
      List.iter2 (TypeTest.assert_ty_equal ~ctxt) args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_arity_var ctxt =
  let args = Clos.ty_bool in
  let res = Clos.ty_float in
  let expected = Clos.arity_var args res in
  match expected with
    | Clos.ArityVar actual ->
      TypeTest.assert_ty_equal ~ctxt args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_builtin_struct_eq ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_struct_eq ty in
  match expected with
    | BuiltinStructEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_struct_neq ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_struct_neq ty in
  match expected with
    | BuiltinStructNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_eq ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_phys_eq ty in
  match expected with
    | BuiltinPhysEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_neq ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_phys_neq ty in
  match expected with
    | BuiltinPhysNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lt ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_lt ty in
  match expected with
    | Clos.BuiltinLt actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lte ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_lte ty in
  match expected with
    | Clos.BuiltinLte actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_gt ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_gt ty in
  match expected with
    | Clos.BuiltinGt actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_gte ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_gte ty in
  match expected with
    | Clos.BuiltinGte actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_add ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_add ty in
  match expected with
    | Clos.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_sub ty in
  match expected with
    | Clos.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_mul ty in
  match expected with
    | Clos.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_div ty in
  match expected with
    | Clos.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Clos.ty_int in
  let expected = Clos.builtin_mod ty in
  match expected with
    | Clos.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Clos.ty_float in
  let expected = Clos.builtin_exp ty in
  match expected with
    | Clos.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_neg ctxt =
  let ty = Clos.ty_float in
  let expected = Clos.builtin_neg ty in
  match expected with
    | Clos.BuiltinNeg actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_and ctxt =
  let ty = Clos.ty_float in
  let expected = Clos.builtin_bit_and ty in
  match expected with
    | Clos.BuiltinBitAnd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_or ctxt =
  let ty = Clos.ty_float in
  let expected = Clos.builtin_bit_or ty in
  match expected with
    | Clos.BuiltinBitOr actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_not ctxt =
  let ty = Clos.ty_float in
  let expected = Clos.builtin_bit_not ty in
  match expected with
    | Clos.BuiltinBitNot actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_xor ctxt =
  let ty = Clos.ty_float in
  let expected = Clos.builtin_bit_xor ty in
  match expected with
    | Clos.BuiltinBitXor actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_log_not ctxt =
  let expected = Clos.builtin_log_not in
  match expected with
    | Clos.BuiltinLogNot -> ()
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Clos.ty_int in
  let sup = Clos.ty_long in
  let expected = Clos.builtin_promote sub sup in
  match expected with
    | Clos.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Clos.ty_string in
  let expected = Clos.builtin_concat ty in
  match expected with
    | Clos.BuiltinConcat actual ->
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

let assert_pp_arity = PrettyTest.assert_pp Clos.pp_arity
let assert_pp_builtin = PrettyTest.assert_pp Clos.pp_builtin

let test_pp_arity_fixed ctxt =
  Clos.arity_fixed [Clos.ty_bool; Clos.ty_int] Clos.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s, %s): %s" Prim.id_bool Prim.id_int Prim.id_float
       ]

let test_pp_arity_var ctxt =
  Clos.arity_var Clos.ty_bool Clos.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s...): %s" Prim.id_bool Prim.id_float
       ]

let test_pp_struct_eq ctxt =
  Clos.builtin_struct_eq Clos.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structEq[%s]" Prim.id_bool
       ]

let test_pp_struct_neq ctxt =
  Clos.builtin_struct_eq Clos.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structNeq[%s]" Prim.id_bool
       ]

let test_pp_phys_eq ctxt =
  Clos.builtin_phys_eq Clos.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physEq[%s]" Prim.id_bool
       ]

let test_pp_phys_neq ctxt =
  Clos.builtin_phys_eq Clos.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physNeq[%s]" Prim.id_bool
       ]

let test_pp_lt ctxt =
  Clos.builtin_lt Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lt[%s]" Prim.id_int
       ]

let test_pp_lte ctxt =
  Clos.builtin_lte Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lte[%s]" Prim.id_int
       ]

let test_pp_gt ctxt =
  Clos.builtin_gt Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gt[%s]" Prim.id_int
       ]

let test_pp_gte ctxt =
  Clos.builtin_gte Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gte[%s]" Prim.id_int
       ]

let test_pp_add ctxt =
  Clos.builtin_add Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "add[%s]" Prim.id_int
       ]

let test_pp_sub ctxt =
  Clos.builtin_sub Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "sub[%s]" Prim.id_int
       ]

let test_pp_mul ctxt =
  Clos.builtin_mul Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mul[%s]" Prim.id_int
       ]

let test_pp_div ctxt =
  Clos.builtin_div Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "div[%s]" Prim.id_int
       ]

let test_pp_mod ctxt =
  Clos.builtin_mod Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mod[%s]" Prim.id_int
       ]

let test_pp_exp ctxt =
  Clos.builtin_exp Clos.ty_float
    |> assert_pp_builtin ~ctxt [
         sprintf "exp[%s]" Prim.id_float
       ]

let test_pp_neg ctxt =
  Clos.builtin_neg Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "neg[%s]" Prim.id_int
       ]

let test_pp_bit_and ctxt =
  Clos.builtin_bit_and Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitAnd[%s]" Prim.id_int
       ]

let test_pp_bit_or ctxt =
  Clos.builtin_bit_or Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitOr[%s]" Prim.id_int
       ]

let test_pp_bit_not ctxt =
  Clos.builtin_bit_not Clos.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitNot[%s]" Prim.id_int
       ]

let test_pp_bit_xor ctxt =
  Clos.builtin_bit_xor Clos.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "bitXor[%s]" Prim.id_bool
       ]

let test_pp_log_not ctxt =
  Clos.builtin_log_not
    |> assert_pp_builtin ~ctxt ["logNot"]

let test_pp_promote ctxt =
  Clos.builtin_promote Clos.ty_int Clos.ty_double
    |> assert_pp_builtin ~ctxt [
         sprintf "logNot[%s, %s]" Prim.id_int Prim.id_double
       ]

let test_pp_concat ctxt =
  Clos.builtin_concat Clos.ty_string
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
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_struct_eq ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_struct_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_struct_neq ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_phys_eq ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_phys_eq ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_phys_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_phys_neq ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_lt ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_lt ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_lte ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_lte ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_gt ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_gt ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_gte ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_gte ty in
  Clos.arity_fixed [ty; ty] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_add ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_add ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_sub ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_sub ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_mul ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_mul ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_div ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_div ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_mod ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_mod ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_exp ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_float in
  let builtin = Clos.builtin_exp ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_neg ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_neg ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_bit_and ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_bit_and ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_bit_or ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_bit_or ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_bit_not ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_bit_not ty in
  Clos.arity_fixed [ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_bit_xor ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_int in
  let builtin = Clos.builtin_bit_xor ty in
  Clos.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_log_not ctxt =
  let env = EnvTest.fresh () in
  let builtin = Clos.builtin_log_not in
  Clos.arity_fixed [Clos.ty_bool] Clos.ty_bool
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_promote ctxt =
  let env = EnvTest.fresh () in
  let sub = Clos.ty_int in
  let sup = Clos.ty_double in
  let builtin = Clos.builtin_promote sub sup in
  Clos.arity_fixed [sub] sup
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

let test_check_concat ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_string in
  let builtin = Clos.builtin_concat ty in
  Clos.arity_var ty ty
    |> assert_arity_equal ~ctxt
    |> Clos.check_builtin env builtin

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
