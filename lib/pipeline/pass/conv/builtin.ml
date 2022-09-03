(* Builtins *)

let rec convert_builtin env builtin kontinue = match builtin with
  | Mono.BuiltinStructEq builtin -> convert_cmp Clos.builtin_struct_eq env builtin.ty kontinue
  | Mono.BuiltinStructNeq builtin -> convert_cmp Clos.builtin_struct_neq env builtin.ty kontinue
  | Mono.BuiltinPhysEq builtin -> convert_cmp Clos.builtin_phys_eq env builtin.ty kontinue
  | Mono.BuiltinPhysNeq builtin -> convert_cmp Clos.builtin_phys_neq env builtin.ty kontinue
  | Mono.BuiltinLt builtin -> convert_cmp Clos.builtin_lt env builtin.ty kontinue
  | Mono.BuiltinLte builtin -> convert_cmp Clos.builtin_lte env builtin.ty kontinue
  | Mono.BuiltinGt builtin -> convert_cmp Clos.builtin_gt env builtin.ty kontinue
  | Mono.BuiltinGte builtin -> convert_cmp Clos.builtin_gte env builtin.ty kontinue
  | Mono.BuiltinLsl builtin -> convert_bin_op Clos.builtin_lsl env builtin.ty kontinue
  | Mono.BuiltinLsr builtin -> convert_bin_op Clos.builtin_lsr env builtin.ty kontinue
  | Mono.BuiltinAsl builtin -> convert_bin_op Clos.builtin_asl env builtin.ty kontinue
  | Mono.BuiltinAsr builtin -> convert_bin_op Clos.builtin_asr env builtin.ty kontinue
  | Mono.BuiltinAdd builtin -> convert_bin_op Clos.builtin_add env builtin.ty kontinue
  | Mono.BuiltinSub builtin -> convert_bin_op Clos.builtin_sub env builtin.ty kontinue
  | Mono.BuiltinMul builtin -> convert_bin_op Clos.builtin_mul env builtin.ty kontinue
  | Mono.BuiltinDiv builtin -> convert_bin_op Clos.builtin_div env builtin.ty kontinue
  | Mono.BuiltinMod builtin -> convert_bin_op Clos.builtin_mod env builtin.ty kontinue
  | Mono.BuiltinExp builtin -> convert_bin_op Clos.builtin_exp env builtin.ty kontinue
  | Mono.BuiltinNeg builtin -> convert_un_op Clos.builtin_neg env builtin.ty kontinue
  | Mono.BuiltinBitAnd builtin -> convert_bin_op Clos.builtin_bit_and env builtin.ty kontinue
  | Mono.BuiltinBitOr builtin -> convert_bin_op Clos.builtin_bit_or env builtin.ty kontinue
  | Mono.BuiltinBitNot builtin -> convert_un_op Clos.builtin_bit_not env builtin.ty kontinue
  | Mono.BuiltinBitXor builtin -> convert_bin_op Clos.builtin_bit_xor env builtin.ty kontinue
  | Mono.BuiltinLogNot -> kontinue (Clos.arity_fixed [Clos.ty_bool] Clos.ty_bool) Clos.builtin_log_not
  | Mono.BuiltinPromote builtin -> convert_promote env builtin.sub builtin.sup kontinue
  | Mono.BuiltinConcat builtin -> convert_concat env builtin.ty kontinue

and convert_cmp op env ty kontinue =
  Type.convert_ty env ty (fun ty ->
    let arity = Clos.arity_fixed [ty; ty] Clos.ty_bool in
    op ty
      |> kontinue arity)

and convert_bin_op op env ty kontinue =
  Type.convert_ty env ty (fun ty ->
    let arity = Clos.arity_fixed [ty; ty] ty in
    op ty
      |> kontinue arity)

and convert_un_op op env ty kontinue =
  Type.convert_ty env ty (fun ty ->
    let arity = Clos.arity_fixed [ty] ty in
    op ty
      |> kontinue arity)

and convert_promote env sub sup kontinue =
  Type.convert_ty env sub (fun sub ->
    Type.convert_ty env sup (fun sup ->
      let arity = Clos.arity_fixed [sub] sup in
      Clos.builtin_promote sub sup
        |> kontinue arity))

and convert_concat env ty kontinue =
  Type.convert_ty env ty (fun ty ->
    let arity = Clos.arity_var ty ty in
    Clos.builtin_concat ty
      |> kontinue arity)
