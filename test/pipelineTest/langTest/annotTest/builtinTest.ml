(* Built-in Functions *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_builtin ?ty:(ty = Annot.ty_bool) _ =
  Annot.builtin_struct_eq ty

(* Assertions *)

let arity_not_equal = TestUtils.not_equal "Arities" Annot.pp_arity
let builtin_not_equal = TestUtils.not_equal "Built-in functions" Annot.pp_builtin

let assert_arity_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.ArityFixed expected, Annot.ArityFixed actual ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Arities are not equal" expected.arity actual.arity;
    List.iter2 (TypeTest.assert_ty_equal ~ctxt) expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | Annot.ArityVar expected, Annot.ArityVar actual ->
    TypeTest.assert_ty_equal ~ctxt expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | expected, actual -> arity_not_equal ~ctxt expected actual

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.BuiltinStructEq expected, Annot.BuiltinStructEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinStructNeq expected, Annot.BuiltinStructNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinPhysEq expected, Annot.BuiltinPhysEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinPhysNeq expected, Annot.BuiltinPhysNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinLt expected, Annot.BuiltinLt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinLte expected, Annot.BuiltinLte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinGt expected, Annot.BuiltinGt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinGte expected, Annot.BuiltinGte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinAdd expected, Annot.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinSub expected, Annot.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinMul expected, Annot.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinDiv expected, Annot.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinMod expected, Annot.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinExp expected, Annot.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinNeg expected, Annot.BuiltinNeg actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinBitAnd expected, Annot.BuiltinBitAnd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinBitOr expected, Annot.BuiltinBitOr actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinBitNot expected, Annot.BuiltinBitNot actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinBitXor expected, Annot.BuiltinBitXor actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinLogNot, Annot.BuiltinLogNot -> ()
  | Annot.BuiltinPromote expected, Annot.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Annot.BuiltinConcat expected, Annot.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_arity_fixed ctxt =
  let args = [Annot.ty_bool; Annot.ty_int] in
  let res = Annot.ty_float in
  let expected = Annot.arity_fixed args res in
  match expected with
    | Annot.ArityFixed actual ->
      assert_equal ~ctxt 2 actual.arity;
      List.iter2 (TypeTest.assert_ty_equal ~ctxt) args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_arity_var ctxt =
  let args = Annot.ty_bool in
  let res = Annot.ty_float in
  let expected = Annot.arity_var args res in
  match expected with
    | Annot.ArityVar actual ->
      TypeTest.assert_ty_equal ~ctxt args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_builtin_struct_eq ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_struct_eq ty in
  match expected with
    | BuiltinStructEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_struct_neq ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_struct_neq ty in
  match expected with
    | BuiltinStructNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_eq ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_phys_eq ty in
  match expected with
    | BuiltinPhysEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_neq ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_phys_neq ty in
  match expected with
    | BuiltinPhysNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lt ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_lt ty in
  match expected with
    | Annot.BuiltinLt actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lte ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_lte ty in
  match expected with
    | Annot.BuiltinLte actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_gt ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_gt ty in
  match expected with
    | Annot.BuiltinGt actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_gte ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_gte ty in
  match expected with
    | Annot.BuiltinGte actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_add ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_add ty in
  match expected with
    | Annot.BuiltinAdd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_sub ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_sub ty in
  match expected with
    | Annot.BuiltinSub actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mul ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_mul ty in
  match expected with
    | Annot.BuiltinMul actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_div ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_div ty in
  match expected with
    | Annot.BuiltinDiv actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_mod ctxt =
  let ty = Annot.ty_int in
  let expected = Annot.builtin_mod ty in
  match expected with
    | Annot.BuiltinMod actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_exp ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_exp ty in
  match expected with
    | Annot.BuiltinExp actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_neg ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_neg ty in
  match expected with
    | Annot.BuiltinNeg actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_and ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_bit_and ty in
  match expected with
    | Annot.BuiltinBitAnd actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_or ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_bit_or ty in
  match expected with
    | Annot.BuiltinBitOr actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_not ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_bit_not ty in
  match expected with
    | Annot.BuiltinBitNot actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_bit_xor ctxt =
  let ty = Annot.ty_float in
  let expected = Annot.builtin_bit_xor ty in
  match expected with
    | Annot.BuiltinBitXor actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_log_not ctxt =
  let expected = Annot.builtin_log_not in
  match expected with
    | Annot.BuiltinLogNot -> ()
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  let sub = Annot.ty_int in
  let sup = Annot.ty_long in
  let expected = Annot.builtin_promote sub sup in
  match expected with
    | Annot.BuiltinPromote actual ->
      TypeTest.assert_ty_equal ~ctxt sub actual.sub;
      TypeTest.assert_ty_equal ~ctxt sup actual.sup;
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_concat ctxt =
  let ty = Annot.ty_string in
  let expected = Annot.builtin_concat ty in
  match expected with
    | Annot.BuiltinConcat actual ->
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

let assert_pp_arity = PrettyTest.assert_pp Annot.pp_arity
let assert_pp_builtin = PrettyTest.assert_pp Annot.pp_builtin

let test_pp_arity_fixed ctxt =
  Annot.arity_fixed [Annot.ty_bool; Annot.ty_int] Annot.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s, %s): %s" Prim.id_bool Prim.id_int Prim.id_float
       ]

let test_pp_arity_var ctxt =
  Annot.arity_var Annot.ty_bool Annot.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s...): %s" Prim.id_bool Prim.id_float
       ]

let test_pp_struct_eq ctxt =
  Annot.builtin_struct_eq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structEq[%s]" Prim.id_bool
       ]

let test_pp_struct_neq ctxt =
  Annot.builtin_struct_eq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structNeq[%s]" Prim.id_bool
       ]

let test_pp_phys_eq ctxt =
  Annot.builtin_phys_eq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physEq[%s]" Prim.id_bool
       ]

let test_pp_phys_neq ctxt =
  Annot.builtin_phys_eq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physNeq[%s]" Prim.id_bool
       ]

let test_pp_lt ctxt =
  Annot.builtin_lt Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lt[%s]" Prim.id_int
       ]

let test_pp_lte ctxt =
  Annot.builtin_lte Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lte[%s]" Prim.id_int
       ]

let test_pp_gt ctxt =
  Annot.builtin_gt Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gt[%s]" Prim.id_int
       ]

let test_pp_gte ctxt =
  Annot.builtin_gte Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gte[%s]" Prim.id_int
       ]

let test_pp_add ctxt =
  Annot.builtin_add Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "add[%s]" Prim.id_int
       ]

let test_pp_sub ctxt =
  Annot.builtin_sub Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "sub[%s]" Prim.id_int
       ]

let test_pp_mul ctxt =
  Annot.builtin_mul Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mul[%s]" Prim.id_int
       ]

let test_pp_div ctxt =
  Annot.builtin_div Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "div[%s]" Prim.id_int
       ]

let test_pp_mod ctxt =
  Annot.builtin_mod Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mod[%s]" Prim.id_int
       ]

let test_pp_exp ctxt =
  Annot.builtin_exp Annot.ty_float
    |> assert_pp_builtin ~ctxt [
         sprintf "exp[%s]" Prim.id_float
       ]

let test_pp_neg ctxt =
  Annot.builtin_neg Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "neg[%s]" Prim.id_int
       ]

let test_pp_bit_and ctxt =
  Annot.builtin_bit_and Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitAnd[%s]" Prim.id_int
       ]

let test_pp_bit_or ctxt =
  Annot.builtin_bit_or Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitOr[%s]" Prim.id_int
       ]

let test_pp_bit_not ctxt =
  Annot.builtin_bit_not Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitNot[%s]" Prim.id_int
       ]

let test_pp_bit_xor ctxt =
  Annot.builtin_bit_xor Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "bitXor[%s]" Prim.id_bool
       ]

let test_pp_log_not ctxt =
  Annot.builtin_log_not
    |> assert_pp_builtin ~ctxt ["logNot"]

let test_pp_promote ctxt =
  Annot.builtin_promote Annot.ty_int Annot.ty_double
    |> assert_pp_builtin ~ctxt [
         sprintf "logNot[%s, %s]" Prim.id_int Prim.id_double
       ]

let test_pp_concat ctxt =
  Annot.builtin_concat Annot.ty_string
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
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_struct_eq ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_struct_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_struct_neq ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_phys_eq ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_phys_eq ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_phys_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_phys_neq ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_lt ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_lt ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_lte ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_lte ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_gt ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_gt ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_gte ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_gte ty in
  Annot.arity_fixed [ty; ty] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_add ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_add ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_sub ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_sub ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_mul ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_mul ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_div ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_div ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_mod ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_mod ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_exp ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_float in
  let builtin = Annot.builtin_exp ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_neg ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_neg ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_bit_and ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_bit_and ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_bit_or ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_bit_or ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_bit_not ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_bit_not ty in
  Annot.arity_fixed [ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_bit_xor ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_bit_xor ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_log_not ctxt =
  let env = EnvTest.fresh () in
  let builtin = Annot.builtin_log_not in
  Annot.arity_fixed [Annot.ty_bool] Annot.ty_bool
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_promote ctxt =
  let env = EnvTest.fresh () in
  let sub = Annot.ty_int in
  let sup = Annot.ty_double in
  let builtin = Annot.builtin_promote sub sup in
  Annot.arity_fixed [sub] sup
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_concat ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_string in
  let builtin = Annot.builtin_concat ty in
  Annot.arity_var ty ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

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
