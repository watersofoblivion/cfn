(* Built-in Functions *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_builtin_struct_eq ?ty:(ty = Annot.ty_bool) _ =
  Annot.builtin_struct_eq ty

let fresh_builtin_struct_neq ?ty:(ty = Annot.ty_bool) _ =
  Annot.builtin_struct_neq ty

let fresh_builtin_phys_eq ?ty:(ty = Annot.ty_bool) _ =
  Annot.builtin_phys_eq ty

let fresh_builtin_phys_neq ?ty:(ty = Annot.ty_bool) _ =
  Annot.builtin_phys_neq ty

let fresh_builtin_lt ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_lt ty

let fresh_builtin_lte ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_lte ty

let fresh_builtin_gt ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_gt ty

let fresh_builtin_gte ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_gte ty

let fresh_builtin_lsl ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_lsl ty

let fresh_builtin_lsr ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_lsr ty

let fresh_builtin_asl ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_asl ty

let fresh_builtin_asr ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_asr ty

let fresh_builtin_add ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_add ty

let fresh_builtin_sub ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_sub ty

let fresh_builtin_mul ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_mul ty

let fresh_builtin_div ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_div ty

let fresh_builtin_mod ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_mod ty

let fresh_builtin_exp ?ty:(ty = Annot.ty_float) _ =
  Annot.builtin_exp ty

let fresh_builtin_neg ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_neg ty

let fresh_builtin_bit_and ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_bit_and ty

let fresh_builtin_bit_or ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_bit_or ty

let fresh_builtin_bit_not ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_bit_not ty

let fresh_builtin_bit_xor ?ty:(ty = Annot.ty_int) _ =
  Annot.builtin_bit_xor ty

let fresh_builtin_log_not _ =
  Annot.builtin_log_not

let fresh_builtin_promote ?sub:(sub = Annot.ty_int) ?sup:(sup = Annot.ty_long) _ =
  Annot.builtin_promote sub sup

let fresh_builtin_concat ?ty:(ty = Annot.ty_string) _ =
  Annot.builtin_concat ty

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
  | Annot.BuiltinLsl expected, Annot.BuiltinLsl actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinLsr expected, Annot.BuiltinLsr actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinAsl expected, Annot.BuiltinAsl actual ->
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty
  | Annot.BuiltinAsr expected, Annot.BuiltinAsr actual ->
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


let assert_limited : (Annot.ty -> bool) -> (Annot.ty -> 'a) -> (Annot.ty -> Annot.builtin) -> (Annot.ty -> Annot.builtin -> unit) -> unit = fun filter ex constr test ->
  let (valid, invalid) = List.partition filter AnnotUtils.types in

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

let assert_numeric = assert_limited Annot.ty_is_numeric (fun ty -> Annot.NotNumeric ty)
let assert_integral = assert_limited Annot.ty_is_integral (fun ty -> Annot.NotIntegral ty)
let assert_floating_point = assert_limited Annot.ty_is_floating_point (fun ty -> Annot.NotFloatingPoint ty)
let assert_string = assert_limited (function Annot.TyString -> true | _ -> false) (fun ty -> Annot.UnsupportedConcatType ty)

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
  assert_numeric Annot.builtin_lt (fun ty expected ->
    match expected with
      | Annot.BuiltinLt actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_lte ctxt =
  assert_numeric Annot.builtin_lte (fun ty expected ->
    match expected with
      | Annot.BuiltinLte actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_gt ctxt =
  assert_numeric Annot.builtin_gt (fun ty expected ->
    match expected with
      | Annot.BuiltinGt actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
        | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_gte ctxt =
  assert_numeric Annot.builtin_gte (fun ty expected ->
    match expected with
      | Annot.BuiltinGte actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_lsl ctxt =
  assert_integral Annot.builtin_lsl (fun ty expected ->
    match expected with
      | Annot.BuiltinLsl actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_lsr ctxt =
  assert_integral Annot.builtin_lsr (fun ty expected ->
    match expected with
      | Annot.BuiltinLsr actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_asl ctxt =
  assert_integral Annot.builtin_asl (fun ty expected ->
    match expected with
      | Annot.BuiltinAsl actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_asr ctxt =
  assert_integral Annot.builtin_asr (fun ty expected ->
    match expected with
      | Annot.BuiltinAsr actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_add ctxt =
  assert_numeric Annot.builtin_add (fun ty expected ->
    match expected with
      | Annot.BuiltinAdd actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_sub ctxt =
  assert_numeric Annot.builtin_sub (fun ty expected ->
    match expected with
      | Annot.BuiltinSub actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_mul ctxt =
  assert_numeric Annot.builtin_mul (fun ty expected ->
    match expected with
      | Annot.BuiltinMul actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_div ctxt =
  assert_numeric Annot.builtin_div (fun ty expected ->
    match expected with
      | Annot.BuiltinDiv actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_mod ctxt =
  assert_integral Annot.builtin_mod (fun ty expected ->
    match expected with
      | Annot.BuiltinMod actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_exp ctxt =
  assert_floating_point Annot.builtin_exp (fun ty expected ->
    match expected with
      | Annot.BuiltinExp actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_neg ctxt =
  assert_numeric Annot.builtin_neg (fun ty expected ->
    match expected with
      | Annot.BuiltinNeg actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_and ctxt =
  assert_integral Annot.builtin_bit_and (fun ty expected ->
    match expected with
      | Annot.BuiltinBitAnd actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_or ctxt =
  assert_integral Annot.builtin_bit_or (fun ty expected ->
    match expected with
      | Annot.BuiltinBitOr actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_not ctxt =
  assert_integral Annot.builtin_bit_not (fun ty expected ->
    match expected with
      | Annot.BuiltinBitNot actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_bit_xor ctxt =
  assert_integral Annot.builtin_bit_xor (fun ty expected ->
    match expected with
      | Annot.BuiltinBitXor actual ->
        TypeTest.assert_ty_equal ~ctxt ty actual.ty
      | actual -> builtin_not_equal ~ctxt expected actual)

let test_builtin_log_not ctxt =
  let expected = Annot.builtin_log_not in
  match expected with
    | Annot.BuiltinLogNot -> ()
    | actual -> builtin_not_equal ~ctxt expected actual

let test_builtin_promote ctxt =
  List.iter (fun sub ->
    List.iter (fun sup ->
      if List.mem (sub, sup) AnnotUtils.valid_promotions
      then
        let expected = Annot.builtin_promote sub sup in
        match expected with
          | Annot.BuiltinPromote actual ->
            TypeTest.assert_ty_equal ~ctxt sub actual.sub;
            TypeTest.assert_ty_equal ~ctxt sup actual.sup;
          | actual -> builtin_not_equal ~ctxt expected actual
      else
        let exn = Annot.UnsupportedPromotion (sub, sup) in
        assert_raises exn (fun _ ->
          Annot.builtin_promote sub sup)
    ) AnnotUtils.types
  ) AnnotUtils.types

let test_builtin_concat ctxt =
  assert_string Annot.builtin_concat (fun ty expected ->
    match expected with
      | Annot.BuiltinConcat actual ->
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
    "Shift" >::: [
      "Logical" >::: [
        "Left"  >:: test_builtin_lsl;
        "Right" >:: test_builtin_lsr;
      ];
      "Arithmetic" >::: [
        "Left"  >:: test_builtin_asl;
        "Right" >:: test_builtin_asr;
      ];
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
         sprintf "_.StructEq[%s]" Prim.id_bool
       ]

let test_pp_struct_neq ctxt =
  Annot.builtin_struct_neq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "_.StructNeq[%s]" Prim.id_bool
       ]

let test_pp_phys_eq ctxt =
  Annot.builtin_phys_eq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "_.PhysEq[%s]" Prim.id_bool
       ]

let test_pp_phys_neq ctxt =
  Annot.builtin_phys_neq Annot.ty_bool
    |> assert_pp_builtin ~ctxt [
         sprintf "_.PhysNeq[%s]" Prim.id_bool
       ]

let test_pp_lt ctxt =
  Annot.builtin_lt Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Lt[%s]" Prim.id_int
       ]

let test_pp_lte ctxt =
  Annot.builtin_lte Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Lte[%s]" Prim.id_int
       ]

let test_pp_gt ctxt =
  Annot.builtin_gt Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Gt[%s]" Prim.id_int
       ]

let test_pp_gte ctxt =
  Annot.builtin_gte Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Gte[%s]" Prim.id_int
       ]

let test_pp_lsl ctxt =
  Annot.builtin_lsl Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Lsl[%s]" Prim.id_int
       ]

let test_pp_lsr ctxt =
  Annot.builtin_lsr Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Lsr[%s]" Prim.id_int
       ]

let test_pp_asl ctxt =
  Annot.builtin_asl Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Asl[%s]" Prim.id_int
       ]

let test_pp_asr ctxt =
  Annot.builtin_asr Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Asr[%s]" Prim.id_int
       ]

let test_pp_add ctxt =
  Annot.builtin_add Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Add[%s]" Prim.id_int
       ]

let test_pp_sub ctxt =
  Annot.builtin_sub Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Sub[%s]" Prim.id_int
       ]

let test_pp_mul ctxt =
  Annot.builtin_mul Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Mul[%s]" Prim.id_int
       ]

let test_pp_div ctxt =
  Annot.builtin_div Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Div[%s]" Prim.id_int
       ]

let test_pp_mod ctxt =
  Annot.builtin_mod Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Mod[%s]" Prim.id_int
       ]

let test_pp_exp ctxt =
  Annot.builtin_exp Annot.ty_float
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Exp[%s]" Prim.id_float
       ]

let test_pp_neg ctxt =
  Annot.builtin_neg Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Neg[%s]" Prim.id_int
       ]

let test_pp_bit_and ctxt =
  Annot.builtin_bit_and Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitAnd[%s]" Prim.id_int
       ]

let test_pp_bit_or ctxt =
  Annot.builtin_bit_or Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitOr[%s]" Prim.id_int
       ]

let test_pp_bit_not ctxt =
  Annot.builtin_bit_not Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitNot[%s]" Prim.id_int
       ]

let test_pp_bit_xor ctxt =
  Annot.builtin_bit_xor Annot.ty_int
    |> assert_pp_builtin ~ctxt [
         sprintf "_.BitXor[%s]" Prim.id_int
       ]

let test_pp_log_not ctxt =
  Annot.builtin_log_not
    |> assert_pp_builtin ~ctxt ["_.LogNot"]

let test_pp_promote ctxt =
  Annot.builtin_promote Annot.ty_int Annot.ty_double
    |> assert_pp_builtin ~ctxt [
         sprintf "_.Promote[%s, %s]" Prim.id_int Prim.id_double
       ]

let test_pp_concat ctxt =
  Annot.builtin_concat Annot.ty_string
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
    "Shift" >::: [
      "Logical" >::: [
        "Left"  >:: test_pp_lsl;
        "Right" >:: test_pp_lsr;
      ];
      "Arithmetic" >::: [
        "Left"  >:: test_pp_asl;
        "Right" >:: test_pp_asr;
      ];
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

let test_check_lsl ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_lsl ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_lsr ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_lsr ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_asl ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_asl ty in
  Annot.arity_fixed [ty; ty] ty
    |> assert_arity_equal ~ctxt
    |> Annot.check_builtin env builtin

let test_check_asr ctxt =
  let env = EnvTest.fresh () in
  let ty = Annot.ty_int in
  let builtin = Annot.builtin_asr ty in
  Annot.arity_fixed [ty; ty] ty
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
  Annot.arity_fixed [ty] ty
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
    "Shift" >::: [
      "Logical" >::: [
        "Left"  >:: test_check_lsl;
        "Right" >:: test_check_lsr;
      ];
      "Arithmetic" >::: [
        "Left"  >:: test_check_asl;
        "Right" >:: test_check_asr;
      ];
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
