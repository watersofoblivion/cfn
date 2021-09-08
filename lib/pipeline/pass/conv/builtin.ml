(* Builtins *)

let convert constr env ty kontinue =
  Type.convert_ty env ty (fun ty ->
    constr ty
      |> kontinue)

let rec convert_builtin env builtin kontinue = match builtin with
  | Mono.BuiltinStructEq builtin -> convert Clos.builtin_struct_eq env builtin.ty kontinue
  | Mono.BuiltinStructNeq builtin -> convert Clos.builtin_struct_neq env builtin.ty kontinue
  | Mono.BuiltinPhysEq builtin -> convert Clos.builtin_phys_eq env builtin.ty kontinue
  | Mono.BuiltinPhysNeq builtin -> convert Clos.builtin_phys_neq env builtin.ty kontinue
  | Mono.BuiltinLt builtin -> convert Clos.builtin_lt env builtin.ty kontinue
  | Mono.BuiltinLte builtin -> convert Clos.builtin_lte env builtin.ty kontinue
  | Mono.BuiltinGt builtin -> convert Clos.builtin_gt env builtin.ty kontinue
  | Mono.BuiltinGte builtin -> convert Clos.builtin_gte env builtin.ty kontinue
  | Mono.BuiltinAdd builtin -> convert Clos.builtin_add env builtin.ty kontinue
  | Mono.BuiltinSub builtin -> convert Clos.builtin_sub env builtin.ty kontinue
  | Mono.BuiltinMul builtin -> convert Clos.builtin_mul env builtin.ty kontinue
  | Mono.BuiltinDiv builtin -> convert Clos.builtin_div env builtin.ty kontinue
  | Mono.BuiltinMod builtin -> convert Clos.builtin_mod env builtin.ty kontinue
  | Mono.BuiltinExp builtin -> convert Clos.builtin_exp env builtin.ty kontinue
  | Mono.BuiltinNeg builtin -> convert Clos.builtin_neg env builtin.ty kontinue
  | Mono.BuiltinBitAnd builtin -> convert Clos.builtin_bit_and env builtin.ty kontinue
  | Mono.BuiltinBitOr builtin -> convert Clos.builtin_bit_or env builtin.ty kontinue
  | Mono.BuiltinBitNot builtin -> convert Clos.builtin_bit_not env builtin.ty kontinue
  | Mono.BuiltinBitXor builtin -> convert Clos.builtin_bit_xor env builtin.ty kontinue
  | Mono.BuiltinLogNot -> kontinue Clos.builtin_log_not
  | Mono.BuiltinPromote builtin -> convert_builtin_promote env builtin.sub builtin.sup kontinue
  | Mono.BuiltinConcat builtin -> convert Clos.builtin_concat env builtin.ty kontinue

and convert_builtin_promote env sub sup kontinue =
  Type.convert_ty env sub (fun sub ->
    Type.convert_ty env sup (fun sup ->
      Clos.builtin_promote sub sup
        |> kontinue))
