(* Built-in Functions *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_builtin_struct_eq ?ty:(ty = Mono.ty_bool) _ =
  Mono.builtin_struct_eq ty

let fresh_builtin_struct_neq ?ty:(ty = Mono.ty_bool) _ =
  Mono.builtin_struct_neq ty

let fresh_builtin_phys_eq ?ty:(ty = Mono.ty_bool) _ =
  Mono.builtin_phys_eq ty

let fresh_builtin_phys_neq ?ty:(ty = Mono.ty_bool) _ =
  Mono.builtin_phys_neq ty

let fresh_builtin_lt ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_lt ty

let fresh_builtin_lte ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_lte ty

let fresh_builtin_gt ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_gt ty

let fresh_builtin_gte ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_gte ty

let fresh_builtin_add ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_add ty

let fresh_builtin_sub ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_sub ty

let fresh_builtin_mul ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_mul ty

let fresh_builtin_div ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_div ty

let fresh_builtin_mod ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_mod ty

let fresh_builtin_exp ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_exp ty

let fresh_builtin_neg ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_neg ty

let fresh_builtin_bit_and ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_bit_and ty

let fresh_builtin_bit_or ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_bit_or ty

let fresh_builtin_bit_not ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_bit_not ty

let fresh_builtin_bit_xor ?ty:(ty = Mono.ty_int) _ =
  Mono.builtin_bit_xor ty

let fresh_builtin_log_not _ =
  Mono.builtin_log_not

let fresh_builtin_promote ?sub:(sub = Mono.ty_int) ?sup:(sup = Mono.ty_long) _ =
  Mono.builtin_promote sub sup

let fresh_builtin_concat ?ty:(ty = Mono.ty_string) _ =
  Mono.builtin_concat ty

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


let assert_limited : (Mono.ty -> bool) -> (Mono.ty -> 'a) -> (Mono.ty -> Mono.builtin) -> (Mono.ty -> Mono.builtin -> unit) -> unit = fun filter ex constr test ->
  let (valid, invalid) = List.partition filter MonoUtils.types in

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

let assert_numeric = assert_limited Mono.ty_is_numeric (fun ty -> Mono.NotNumeric ty)
let assert_integral = assert_limited Mono.ty_is_integral (fun ty -> Mono.NotIntegral ty)
let assert_floating_point = assert_limited Mono.ty_is_floating_point (fun ty -> Mono.NotFloatingPoint ty)
let assert_string = assert_limited (function Mono.TyString -> true | _ -> false) (fun ty -> Mono.UnsupportedConcatType ty)

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
  assert_numeric Mono.builtin_lt (fun ty expected ->
    match expected with
      | Mono.BuiltinLt actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)


let test_builtin_lte ctxt =
  assert_numeric Mono.builtin_lte (fun ty expected ->
    match expected with
      | Mono.BuiltinLte actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_gt ctxt =
  assert_numeric Mono.builtin_gt (fun ty expected ->
    match expected with
      | Mono.BuiltinGt actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
        | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_gte ctxt =
  assert_numeric Mono.builtin_gte (fun ty expected ->
    match expected with
      | Mono.BuiltinGte actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_add ctxt =
  assert_numeric Mono.builtin_add (fun ty expected ->
    match expected with
      | Mono.BuiltinAdd actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_sub ctxt =
  assert_numeric Mono.builtin_sub (fun ty expected ->
    match expected with
      | Mono.BuiltinSub actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_mul ctxt =
  assert_numeric Mono.builtin_mul (fun ty expected ->
    match expected with
      | Mono.BuiltinMul actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_div ctxt =
  assert_numeric Mono.builtin_div (fun ty expected ->
    match expected with
      | Mono.BuiltinDiv actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_mod ctxt =
  assert_integral Mono.builtin_mod (fun ty expected ->
    match expected with
      | Mono.BuiltinMod actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_exp ctxt =
  assert_floating_point Mono.builtin_exp (fun ty expected ->
    match expected with
      | Mono.BuiltinExp actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_neg ctxt =
  assert_numeric Mono.builtin_neg (fun ty expected ->
    match expected with
      | Mono.BuiltinNeg actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_and ctxt =
  assert_integral Mono.builtin_bit_and (fun ty expected ->
    match expected with
      | Mono.BuiltinBitAnd actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_or ctxt =
  assert_integral Mono.builtin_bit_or (fun ty expected ->
    match expected with
      | Mono.BuiltinBitOr actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_not ctxt =
  assert_integral Mono.builtin_bit_not (fun ty expected ->
    match expected with
      | Mono.BuiltinBitNot actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_xor ctxt =
  assert_integral Mono.builtin_bit_xor (fun ty expected ->
    match expected with
      | Mono.BuiltinBitXor actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_log_not ctxt =
  let expected = Mono.builtin_log_not in
  match expected with
    | Mono.BuiltinLogNot -> ()
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  List.iter (fun sub ->
    List.iter (fun sup ->
      if List.mem (sub, sup) MonoUtils.valid_promotions
      then
        let expected = Mono.builtin_promote sub sup in
        match expected with
          | Mono.BuiltinPromote actual ->
            TypeTest.assert_ty_equal ~ctxt sub actual.sub;
            TypeTest.assert_ty_equal ~ctxt sup actual.sup;
          | actual -> builtin_not_equal ~ctxt expected actual
      else
        let exn = Mono.UnsupportedPromotion (sub, sup) in
        assert_raises exn (fun _ ->
          Mono.builtin_promote sub sup)
    ) MonoUtils.types
  ) MonoUtils.types

let test_builtin_concat ctxt =
  assert_string Mono.builtin_concat (fun ty expected ->
    match expected with
      | Mono.BuiltinConcat actual ->
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
         sprintf "_.StructEq[%s]" Prim.id_bool
       ]

let test_pp_struct_neq ctxt =
  Mono.builtin_struct_neq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "_.StructNeq[%s]" Prim.id_bool
       ]

let test_pp_phys_eq ctxt =
  Mono.builtin_phys_eq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "_.PhysEq[%s]" Prim.id_bool
       ]

let test_pp_phys_neq ctxt =
  Mono.builtin_phys_neq Mono.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "_.PhysNeq[%s]" Prim.id_bool
       ]

let test_pp_lt ctxt =
  Mono.builtin_lt Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Lt[%s]" Prim.id_int
       ]

let test_pp_lte ctxt =
  Mono.builtin_lte Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Lte[%s]" Prim.id_int
       ]

let test_pp_gt ctxt =
  Mono.builtin_gt Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Gt[%s]" Prim.id_int
       ]

let test_pp_gte ctxt =
  Mono.builtin_gte Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Gte[%s]" Prim.id_int
       ]

let test_pp_add ctxt =
  Mono.builtin_add Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Add[%s]" Prim.id_int
       ]

let test_pp_sub ctxt =
  Mono.builtin_sub Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Sub[%s]" Prim.id_int
       ]

let test_pp_mul ctxt =
  Mono.builtin_mul Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Mul[%s]" Prim.id_int
       ]

let test_pp_div ctxt =
  Mono.builtin_div Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Div[%s]" Prim.id_int
       ]

let test_pp_mod ctxt =
  Mono.builtin_mod Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Mod[%s]" Prim.id_int
       ]

let test_pp_exp ctxt =
  Mono.builtin_exp Mono.ty_float
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Exp[%s]" Prim.id_float
       ]

let test_pp_neg ctxt =
  Mono.builtin_neg Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Neg[%s]" Prim.id_int
       ]

let test_pp_bit_and ctxt =
  Mono.builtin_bit_and Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitAnd[%s]" Prim.id_int
       ]

let test_pp_bit_or ctxt =
  Mono.builtin_bit_or Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitOr[%s]" Prim.id_int
       ]

let test_pp_bit_not ctxt =
  Mono.builtin_bit_not Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitNot[%s]" Prim.id_int
       ]

let test_pp_bit_xor ctxt =
  Mono.builtin_bit_xor Mono.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitXor[%s]" Prim.id_int
       ]

let test_pp_log_not ctxt =
  Mono.builtin_log_not
    |> assert_pp_builtin ~ctxt ["_.LogNot"]

let test_pp_promote ctxt =
  Mono.builtin_promote Mono.ty_int Mono.ty_double
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Promote[%s, %s]" Prim.id_int Prim.id_double
       ]

let test_pp_concat ctxt =
  Mono.builtin_concat Mono.ty_string
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Concat[%s]" Prim.id_string
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
  Mono.arity_fixed [ty] ty
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
