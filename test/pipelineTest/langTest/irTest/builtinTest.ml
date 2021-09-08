(* Built-in Functions *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_builtin_struct_eq ?ty:(ty = Ir.ty_bool) _ =
  Ir.builtin_struct_eq ty

let fresh_builtin_struct_neq ?ty:(ty = Ir.ty_bool) _ =
  Ir.builtin_struct_neq ty

let fresh_builtin_phys_eq ?ty:(ty = Ir.ty_bool) _ =
  Ir.builtin_phys_eq ty

let fresh_builtin_phys_neq ?ty:(ty = Ir.ty_bool) _ =
  Ir.builtin_phys_neq ty

let fresh_builtin_lt ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_lt ty

let fresh_builtin_lte ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_lte ty

let fresh_builtin_gt ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_gt ty

let fresh_builtin_gte ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_gte ty

let fresh_builtin_add ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_add ty

let fresh_builtin_sub ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_sub ty

let fresh_builtin_mul ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_mul ty

let fresh_builtin_div ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_div ty

let fresh_builtin_mod ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_mod ty

let fresh_builtin_exp ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_exp ty

let fresh_builtin_neg ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_neg ty

let fresh_builtin_bit_and ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_bit_and ty

let fresh_builtin_bit_or ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_bit_or ty

let fresh_builtin_bit_not ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_bit_not ty

let fresh_builtin_bit_xor ?ty:(ty = Ir.ty_int) _ =
  Ir.builtin_bit_xor ty

let fresh_builtin_log_not _ =
  Ir.builtin_log_not

let fresh_builtin_promote ?sub:(sub = Ir.ty_int) ?sup:(sup = Ir.ty_long) _ =
  Ir.builtin_promote sub sup

let fresh_builtin_concat ?ty:(ty = Ir.ty_string) _ =
  Ir.builtin_concat ty

(* Assertions *)

let arity_not_equal = TestUtils.not_equal "Arities" Ir.pp_arity
let builtin_not_equal = TestUtils.not_equal "Built-in functions" Ir.pp_builtin

let assert_arity_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.ArityFixed expected, Ir.ArityFixed actual ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Arities are not equal" expected.arity actual.arity;
    List.iter2 (TypeTest.assert_ty_equal ~ctxt) expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | Ir.ArityVar expected, Ir.ArityVar actual ->
    TypeTest.assert_ty_equal ~ctxt expected.args actual.args;
    TypeTest.assert_ty_equal ~ctxt expected.res actual.res
  | expected, actual -> arity_not_equal ~ctxt expected actual

let assert_builtin_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.BuiltinStructEq expected, Ir.BuiltinStructEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinStructNeq expected, Ir.BuiltinStructNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinPhysEq expected, Ir.BuiltinPhysEq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinPhysNeq expected, Ir.BuiltinPhysNeq actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinLt expected, Ir.BuiltinLt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinLte expected, Ir.BuiltinLte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinGt expected, Ir.BuiltinGt actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinGte expected, Ir.BuiltinGte actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinAdd expected, Ir.BuiltinAdd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinSub expected, Ir.BuiltinSub actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinMul expected, Ir.BuiltinMul actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinDiv expected, Ir.BuiltinDiv actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinMod expected, Ir.BuiltinMod actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinExp expected, Ir.BuiltinExp actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinNeg expected, Ir.BuiltinNeg actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinBitAnd expected, Ir.BuiltinBitAnd actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinBitOr expected, Ir.BuiltinBitOr actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinBitNot expected, Ir.BuiltinBitNot actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinBitXor expected, Ir.BuiltinBitXor actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Ir.BuiltinLogNot, Ir.BuiltinLogNot -> ()
  | Ir.BuiltinPromote expected, Ir.BuiltinPromote actual ->
    TypeTest.assert_ty_equal ~ctxt expected.sub actual.sub;
    TypeTest.assert_ty_equal ~ctxt expected.sup actual.sup
  | Ir.BuiltinConcat expected, Ir.BuiltinConcat actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | expected, actual -> builtin_not_equal ~ctxt expected actual


let assert_limited : (Ir.ty -> bool) -> (Ir.ty -> 'a) -> (Ir.ty -> Ir.builtin) -> (Ir.ty -> Ir.builtin -> unit) -> unit = fun filter ex constr test ->
  let (valid, invalid) = List.partition filter IrUtils.types in

  let assert_valid ty =
    constr ty
      |> test ty
  in
  List.iter assert_valid valid;

  let assert_invalid ty =
    let exn = ex ty in
    assert_raises exn (fun _ ->
      ignore (constr ty))
  in
  List.iter assert_invalid invalid

let assert_numeric = assert_limited Ir.ty_is_numeric (fun ty -> Ir.NotNumeric ty)
let assert_integral = assert_limited Ir.ty_is_integral (fun ty -> Ir.NotIntegral ty)
let assert_floating_point = assert_limited Ir.ty_is_floating_point (fun ty -> Ir.NotFloatingPoint ty)
let assert_string = assert_limited (function Ir.TyString -> true | _ -> false) (fun ty -> Ir.UnsupportedConcatType ty)

(* Tests *)

(* Constructors *)

let test_arity_fixed ctxt =
  let args = [Ir.ty_bool; Ir.ty_int] in
  let res = Ir.ty_float in
  let expected = Ir.arity_fixed args res in
  match expected with
    | Ir.ArityFixed actual ->
      assert_equal ~ctxt 2 actual.arity;
      List.iter2 (TypeTest.assert_ty_equal ~ctxt) args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_arity_var ctxt =
  let args = Ir.ty_bool in
  let res = Ir.ty_float in
  let expected = Ir.arity_var args res in
  match expected with
    | Ir.ArityVar actual ->
      TypeTest.assert_ty_equal ~ctxt args actual.args;
      TypeTest.assert_ty_equal ~ctxt res actual.res
    | actual -> arity_not_equal ~ctxt expected actual

let test_builtin_struct_eq ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_struct_eq ty in
  match expected with
    | BuiltinStructEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_struct_neq ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_struct_neq ty in
  match expected with
    | BuiltinStructNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_eq ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_phys_eq ty in
  match expected with
    | BuiltinPhysEq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_phys_neq ctxt =
  let ty = Ir.ty_int in
  let expected = Ir.builtin_phys_neq ty in
  match expected with
    | BuiltinPhysNeq actual ->
      TypeTest.assert_ty_equal ~ctxt ty actual.ty
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_lt ctxt =
  assert_numeric Ir.builtin_lt (fun ty expected ->
    match expected with
      | Ir.BuiltinLt actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)


let test_builtin_lte ctxt =
  assert_numeric Ir.builtin_lte (fun ty expected ->
    match expected with
      | Ir.BuiltinLte actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_gt ctxt =
  assert_numeric Ir.builtin_gt (fun ty expected ->
    match expected with
      | Ir.BuiltinGt actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
        | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_gte ctxt =
  assert_numeric Ir.builtin_gte (fun ty expected ->
    match expected with
      | Ir.BuiltinGte actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_add ctxt =
  assert_numeric Ir.builtin_add (fun ty expected ->
    match expected with
      | Ir.BuiltinAdd actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_sub ctxt =
  assert_numeric Ir.builtin_sub (fun ty expected ->
    match expected with
      | Ir.BuiltinSub actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_mul ctxt =
  assert_numeric Ir.builtin_mul (fun ty expected ->
    match expected with
      | Ir.BuiltinMul actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_div ctxt =
  assert_numeric Ir.builtin_div (fun ty expected ->
    match expected with
      | Ir.BuiltinDiv actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_mod ctxt =
  assert_integral Ir.builtin_mod (fun ty expected ->
    match expected with
      | Ir.BuiltinMod actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_exp ctxt =
  assert_floating_point Ir.builtin_exp (fun ty expected ->
    match expected with
      | Ir.BuiltinExp actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_neg ctxt =
  assert_numeric Ir.builtin_neg (fun ty expected ->
    match expected with
      | Ir.BuiltinNeg actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_and ctxt =
  assert_integral Ir.builtin_bit_and (fun ty expected ->
    match expected with
      | Ir.BuiltinBitAnd actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_or ctxt =
  assert_integral Ir.builtin_bit_or (fun ty expected ->
    match expected with
      | Ir.BuiltinBitOr actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_not ctxt =
  assert_integral Ir.builtin_bit_not (fun ty expected ->
    match expected with
      | Ir.BuiltinBitNot actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_xor ctxt =
  assert_integral Ir.builtin_bit_xor (fun ty expected ->
    match expected with
      | Ir.BuiltinBitXor actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_log_not ctxt =
  let expected = Ir.builtin_log_not in
  match expected with
    | Ir.BuiltinLogNot -> ()
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  List.iter (fun sub ->
    List.iter (fun sup ->
      if List.mem (sub, sup) IrUtils.valid_promotions
      then
        let expected = Ir.builtin_promote sub sup in
        match expected with
          | Ir.BuiltinPromote actual ->
            TypeTest.assert_ty_equal ~ctxt sub actual.sub;
            TypeTest.assert_ty_equal ~ctxt sup actual.sup;
          | actual -> builtin_not_equal ~ctxt expected actual
      else
        let exn = Ir.UnsupportedPromotion (sub, sup) in
        assert_raises exn (fun _ ->
          Ir.builtin_promote sub sup)
    ) IrUtils.types
  ) IrUtils.types

let test_builtin_concat ctxt =
  assert_string Ir.builtin_concat (fun ty expected ->
    match expected with
      | Ir.BuiltinConcat actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

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

let assert_pp_arity = PrettyTest.assert_pp Ir.pp_arity
let assert_pp_builtin = PrettyTest.assert_pp Ir.pp_builtin

let test_pp_arity_fixed ctxt =
  Ir.arity_fixed [Ir.ty_bool; Ir.ty_int] Ir.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s, %s): %s" Prim.id_bool Prim.id_int Prim.id_float
       ]

let test_pp_arity_var ctxt =
  Ir.arity_var Ir.ty_bool Ir.ty_float
    |> assert_pp_arity ~ctxt [
         sprintf "(%s...): %s" Prim.id_bool Prim.id_float
       ]

let test_pp_struct_eq ctxt =
  Ir.builtin_struct_eq Ir.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structEq[%s]" Prim.id_bool
       ]

let test_pp_struct_neq ctxt =
  Ir.builtin_struct_neq Ir.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "structNeq[%s]" Prim.id_bool
       ]

let test_pp_phys_eq ctxt =
  Ir.builtin_phys_eq Ir.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physEq[%s]" Prim.id_bool
       ]

let test_pp_phys_neq ctxt =
  Ir.builtin_phys_neq Ir.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "physNeq[%s]" Prim.id_bool
       ]

let test_pp_lt ctxt =
  Ir.builtin_lt Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lt[%s]" Prim.id_int
       ]

let test_pp_lte ctxt =
  Ir.builtin_lte Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "lte[%s]" Prim.id_int
       ]

let test_pp_gt ctxt =
  Ir.builtin_gt Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gt[%s]" Prim.id_int
       ]

let test_pp_gte ctxt =
  Ir.builtin_gte Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "gte[%s]" Prim.id_int
       ]

let test_pp_add ctxt =
  Ir.builtin_add Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "add[%s]" Prim.id_int
       ]

let test_pp_sub ctxt =
  Ir.builtin_sub Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "sub[%s]" Prim.id_int
       ]

let test_pp_mul ctxt =
  Ir.builtin_mul Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mul[%s]" Prim.id_int
       ]

let test_pp_div ctxt =
  Ir.builtin_div Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "div[%s]" Prim.id_int
       ]

let test_pp_mod ctxt =
  Ir.builtin_mod Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "mod[%s]" Prim.id_int
       ]

let test_pp_exp ctxt =
  Ir.builtin_exp Ir.ty_float
    |> assert_pp_builtin ~ctxt [
         sprintf "exp[%s]" Prim.id_float
       ]

let test_pp_neg ctxt =
  Ir.builtin_neg Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "neg[%s]" Prim.id_int
       ]

let test_pp_bit_and ctxt =
  Ir.builtin_bit_and Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitAnd[%s]" Prim.id_int
       ]

let test_pp_bit_or ctxt =
  Ir.builtin_bit_or Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitOr[%s]" Prim.id_int
       ]

let test_pp_bit_not ctxt =
  Ir.builtin_bit_not Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitNot[%s]" Prim.id_int
       ]

let test_pp_bit_xor ctxt =
  Ir.builtin_bit_xor Ir.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "bitXor[%s]" Prim.id_bool
       ]

let test_pp_log_not ctxt =
  Ir.builtin_log_not
    |> assert_pp_builtin ~ctxt ["logNot"]

let test_pp_promote ctxt =
  Ir.builtin_promote Ir.ty_int Ir.ty_double
    |> assert_pp_builtin ~ctxt [
         sprintf "logNot[%s, %s]" Prim.id_int Prim.id_double
       ]

let test_pp_concat ctxt =
  Ir.builtin_concat Ir.ty_string
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
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_struct_eq ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_struct_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_struct_neq ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_phys_eq ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_phys_eq ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_phys_neq ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_phys_neq ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_lt ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_lt ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_lte ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_lte ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_gt ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_gt ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_gte ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_gte ty in
  Ir.arity_fixed [ty; ty] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_add ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_add ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_sub ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_sub ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_mul ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_mul ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_div ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_div ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_mod ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_mod ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_exp ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_float in
  let builtin = Ir.builtin_exp ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_neg ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_neg ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_bit_and ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_bit_and ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_bit_or ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_bit_or ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_bit_not ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_bit_not ty in
  Ir.arity_fixed [ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_bit_xor ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_int in
  let builtin = Ir.builtin_bit_xor ty in
  Ir.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_log_not ctxt =
  let env = EnvTest.fresh () in
  let builtin = Ir.builtin_log_not in
  Ir.arity_fixed [Ir.ty_bool] Ir.ty_bool
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_promote ctxt =
  let env = EnvTest.fresh () in
  let sub = Ir.ty_int in
  let sup = Ir.ty_double in
  let builtin = Ir.builtin_promote sub sup in
  Ir.arity_fixed [sub] sup
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

let test_check_concat ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_string in
  let builtin = Ir.builtin_concat ty in
  Ir.arity_var ty ty
    |> assert_arity_equal ~ctxt
    |> Ir.check_builtin env builtin

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
