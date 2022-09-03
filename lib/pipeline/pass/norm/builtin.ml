(* Builtins *)

let rec norm_builtin env builtin kontinue = match builtin with
  | Annot.BuiltinStructEq builtin -> norm_cmp Ir.builtin_struct_eq env builtin.ty kontinue
  | Annot.BuiltinStructNeq builtin -> norm_cmp Ir.builtin_struct_neq env builtin.ty kontinue
  | Annot.BuiltinPhysEq builtin -> norm_cmp Ir.builtin_phys_eq env builtin.ty kontinue
  | Annot.BuiltinPhysNeq builtin -> norm_cmp Ir.builtin_phys_neq env builtin.ty kontinue
  | Annot.BuiltinLt builtin -> norm_cmp Ir.builtin_lt env builtin.ty kontinue
  | Annot.BuiltinLte builtin -> norm_cmp Ir.builtin_lte env builtin.ty kontinue
  | Annot.BuiltinGt builtin -> norm_cmp Ir.builtin_gt env builtin.ty kontinue
  | Annot.BuiltinGte builtin -> norm_cmp Ir.builtin_gte env builtin.ty kontinue
  | Annot.BuiltinLsl builtin -> norm_bin_op Ir.builtin_lsl env builtin.ty kontinue
  | Annot.BuiltinLsr builtin -> norm_bin_op Ir.builtin_lsr env builtin.ty kontinue
  | Annot.BuiltinAsl builtin -> norm_bin_op Ir.builtin_asl env builtin.ty kontinue
  | Annot.BuiltinAsr builtin -> norm_bin_op Ir.builtin_asr env builtin.ty kontinue
  | Annot.BuiltinAdd builtin -> norm_bin_op Ir.builtin_add env builtin.ty kontinue
  | Annot.BuiltinSub builtin -> norm_bin_op Ir.builtin_sub env builtin.ty kontinue
  | Annot.BuiltinMul builtin -> norm_bin_op Ir.builtin_mul env builtin.ty kontinue
  | Annot.BuiltinDiv builtin -> norm_bin_op Ir.builtin_div env builtin.ty kontinue
  | Annot.BuiltinMod builtin -> norm_bin_op Ir.builtin_mod env builtin.ty kontinue
  | Annot.BuiltinExp builtin -> norm_bin_op Ir.builtin_exp env builtin.ty kontinue
  | Annot.BuiltinNeg builtin -> norm_un_op Ir.builtin_neg env builtin.ty kontinue
  | Annot.BuiltinBitAnd builtin -> norm_bin_op Ir.builtin_bit_and env builtin.ty kontinue
  | Annot.BuiltinBitOr builtin -> norm_bin_op Ir.builtin_bit_or env builtin.ty kontinue
  | Annot.BuiltinBitNot builtin -> norm_un_op Ir.builtin_bit_not env builtin.ty kontinue
  | Annot.BuiltinBitXor builtin -> norm_bin_op Ir.builtin_bit_xor env builtin.ty kontinue
  | Annot.BuiltinLogNot -> kontinue (Ir.arity_fixed [Ir.ty_bool] Ir.ty_bool) Ir.builtin_log_not
  | Annot.BuiltinPromote builtin -> norm_promote env builtin.sub builtin.sup kontinue
  | Annot.BuiltinConcat builtin -> norm_concat env builtin.ty kontinue

and norm_cmp op env ty kontinue =
  Type.norm_ty env ty (fun ty ->
    let arity = Ir.arity_fixed [ty; ty] Ir.ty_bool in
    op ty
      |> kontinue arity)

and norm_bin_op op env ty kontinue =
  Type.norm_ty env ty (fun ty ->
    let arity = Ir.arity_fixed [ty; ty] ty in
    op ty
      |> kontinue arity)

and norm_un_op op env ty kontinue =
  Type.norm_ty env ty (fun ty ->
    let arity = Ir.arity_fixed [ty] ty in
    op ty
      |> kontinue arity)

and norm_promote env sub sup kontinue =
  Type.norm_ty env sub (fun sub ->
    Type.norm_ty env sup (fun sup ->
      let arity = Ir.arity_fixed [sub] sup in
      Ir.builtin_promote sub sup
        |> kontinue arity))

and norm_concat env ty kontinue =
  Type.norm_ty env ty (fun ty ->
    let arity = Ir.arity_var ty ty in
    Ir.builtin_concat ty
      |> kontinue arity)
