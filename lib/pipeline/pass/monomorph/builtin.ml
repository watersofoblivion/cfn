(* Builtins *)

let convert constr env ty kontinue =
  Type.convert_ty env ty (fun ty ->
    constr ty
      |> kontinue)

let rec convert_builtin env builtin kontinue = match builtin with
  | Ir.BuiltinStructEq builtin -> convert Mono.builtin_struct_eq env builtin.ty kontinue
  | Ir.BuiltinStructNeq builtin -> convert Mono.builtin_struct_neq env builtin.ty kontinue
  | Ir.BuiltinPhysEq builtin -> convert Mono.builtin_phys_eq env builtin.ty kontinue
  | Ir.BuiltinPhysNeq builtin -> convert Mono.builtin_phys_neq env builtin.ty kontinue
  | Ir.BuiltinLt builtin -> convert Mono.builtin_lt env builtin.ty kontinue
  | Ir.BuiltinLte builtin -> convert Mono.builtin_lte env builtin.ty kontinue
  | Ir.BuiltinGt builtin -> convert Mono.builtin_gt env builtin.ty kontinue
  | Ir.BuiltinGte builtin -> convert Mono.builtin_gte env builtin.ty kontinue
  | Ir.BuiltinLsl builtin -> convert Mono.builtin_lsl env builtin.ty kontinue
  | Ir.BuiltinLsr builtin -> convert Mono.builtin_lsr env builtin.ty kontinue
  | Ir.BuiltinAsl builtin -> convert Mono.builtin_asl env builtin.ty kontinue
  | Ir.BuiltinAsr builtin -> convert Mono.builtin_asr env builtin.ty kontinue
  | Ir.BuiltinAdd builtin -> convert Mono.builtin_add env builtin.ty kontinue
  | Ir.BuiltinSub builtin -> convert Mono.builtin_sub env builtin.ty kontinue
  | Ir.BuiltinMul builtin -> convert Mono.builtin_mul env builtin.ty kontinue
  | Ir.BuiltinDiv builtin -> convert Mono.builtin_div env builtin.ty kontinue
  | Ir.BuiltinMod builtin -> convert Mono.builtin_mod env builtin.ty kontinue
  | Ir.BuiltinExp builtin -> convert Mono.builtin_exp env builtin.ty kontinue
  | Ir.BuiltinNeg builtin -> convert Mono.builtin_neg env builtin.ty kontinue
  | Ir.BuiltinBitAnd builtin -> convert Mono.builtin_bit_and env builtin.ty kontinue
  | Ir.BuiltinBitOr builtin -> convert Mono.builtin_bit_or env builtin.ty kontinue
  | Ir.BuiltinBitNot builtin -> convert Mono.builtin_bit_not env builtin.ty kontinue
  | Ir.BuiltinBitXor builtin -> convert Mono.builtin_bit_xor env builtin.ty kontinue
  | Ir.BuiltinLogNot -> kontinue Mono.builtin_log_not
  | Ir.BuiltinPromote builtin -> convert_builtin_promote env builtin.sub builtin.sup kontinue
  | Ir.BuiltinConcat builtin -> convert Mono.builtin_concat env builtin.ty kontinue

and convert_builtin_promote env sub sup kontinue =
  Type.convert_ty env sub (fun sub ->
    Type.convert_ty env sup (fun sup ->
      Mono.builtin_promote sub sup
        |> kontinue))
