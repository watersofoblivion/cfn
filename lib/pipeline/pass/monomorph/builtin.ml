(* Builtins *)

let rec mono_builtin env builtin kontinue = match builtin with
  | Ir.BuiltinStructEq builtin -> mono_cmp Mono.builtin_struct_eq env builtin.ty kontinue
  | Ir.BuiltinStructNeq builtin -> mono_cmp Mono.builtin_struct_neq env builtin.ty kontinue
  | Ir.BuiltinPhysEq builtin -> mono_cmp Mono.builtin_phys_eq env builtin.ty kontinue
  | Ir.BuiltinPhysNeq builtin -> mono_cmp Mono.builtin_phys_neq env builtin.ty kontinue
  | Ir.BuiltinLt builtin -> mono_cmp Mono.builtin_lt env builtin.ty kontinue
  | Ir.BuiltinLte builtin -> mono_cmp Mono.builtin_lte env builtin.ty kontinue
  | Ir.BuiltinGt builtin -> mono_cmp Mono.builtin_gt env builtin.ty kontinue
  | Ir.BuiltinGte builtin -> mono_cmp Mono.builtin_gte env builtin.ty kontinue
  | Ir.BuiltinLsl builtin -> mono_bin_op Mono.builtin_lsl env builtin.ty kontinue
  | Ir.BuiltinLsr builtin -> mono_bin_op Mono.builtin_lsr env builtin.ty kontinue
  | Ir.BuiltinAsl builtin -> mono_bin_op Mono.builtin_asl env builtin.ty kontinue
  | Ir.BuiltinAsr builtin -> mono_bin_op Mono.builtin_asr env builtin.ty kontinue
  | Ir.BuiltinAdd builtin -> mono_bin_op Mono.builtin_add env builtin.ty kontinue
  | Ir.BuiltinSub builtin -> mono_bin_op Mono.builtin_sub env builtin.ty kontinue
  | Ir.BuiltinMul builtin -> mono_bin_op Mono.builtin_mul env builtin.ty kontinue
  | Ir.BuiltinDiv builtin -> mono_bin_op Mono.builtin_div env builtin.ty kontinue
  | Ir.BuiltinMod builtin -> mono_bin_op Mono.builtin_mod env builtin.ty kontinue
  | Ir.BuiltinExp builtin -> mono_bin_op Mono.builtin_exp env builtin.ty kontinue
  | Ir.BuiltinNeg builtin -> mono_un_op Mono.builtin_neg env builtin.ty kontinue
  | Ir.BuiltinBitAnd builtin -> mono_bin_op Mono.builtin_bit_and env builtin.ty kontinue
  | Ir.BuiltinBitOr builtin -> mono_bin_op Mono.builtin_bit_or env builtin.ty kontinue
  | Ir.BuiltinBitNot builtin -> mono_un_op Mono.builtin_bit_not env builtin.ty kontinue
  | Ir.BuiltinBitXor builtin -> mono_bin_op Mono.builtin_bit_xor env builtin.ty kontinue
  | Ir.BuiltinLogNot -> kontinue (Mono.arity_fixed [Mono.ty_bool] Mono.ty_bool) Mono.builtin_log_not
  | Ir.BuiltinPromote builtin -> mono_promote env builtin.sub builtin.sup kontinue
  | Ir.BuiltinConcat builtin -> mono_concat env builtin.ty kontinue

and mono_cmp op env ty kontinue =
  Type.mono_ty env ty (fun ty ->
    let arity = Mono.arity_fixed [ty; ty] Mono.ty_bool in
    op ty
      |> kontinue arity)

and mono_bin_op op env ty kontinue =
  Type.mono_ty env ty (fun ty ->
    let arity = Mono.arity_fixed [ty; ty] ty in
    op ty
      |> kontinue arity)

and mono_un_op op env ty kontinue =
  Type.mono_ty env ty (fun ty ->
    let arity = Mono.arity_fixed [ty] ty in
    op ty
      |> kontinue arity)

and mono_promote env sub sup kontinue =
  Type.mono_ty env sub (fun sub ->
    Type.mono_ty env sup (fun sup ->
      let arity = Mono.arity_fixed [sub] sup in
      Mono.builtin_promote sub sup
        |> kontinue arity))

and mono_concat env ty kontinue =
  Type.mono_ty env ty (fun ty ->
    let arity = Mono.arity_var ty ty in
    Mono.builtin_concat ty
      |> kontinue arity)
