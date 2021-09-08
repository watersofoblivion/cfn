(* Builtins *)

let norm constr env ty kontinue =
  Type.norm_ty env ty (fun ty ->
    constr ty
      |> kontinue)

let rec norm_builtin env builtin kontinue = match builtin with
  | Annot.BuiltinStructEq builtin -> norm Ir.builtin_struct_eq env builtin.ty kontinue
  | Annot.BuiltinStructNeq builtin -> norm Ir.builtin_struct_neq env builtin.ty kontinue
  | Annot.BuiltinPhysEq builtin -> norm Ir.builtin_phys_eq env builtin.ty kontinue
  | Annot.BuiltinPhysNeq builtin -> norm Ir.builtin_phys_neq env builtin.ty kontinue
  | Annot.BuiltinLt builtin -> norm Ir.builtin_lt env builtin.ty kontinue
  | Annot.BuiltinLte builtin -> norm Ir.builtin_lte env builtin.ty kontinue
  | Annot.BuiltinGt builtin -> norm Ir.builtin_gt env builtin.ty kontinue
  | Annot.BuiltinGte builtin -> norm Ir.builtin_gte env builtin.ty kontinue
  | Annot.BuiltinAdd builtin -> norm Ir.builtin_add env builtin.ty kontinue
  | Annot.BuiltinSub builtin -> norm Ir.builtin_sub env builtin.ty kontinue
  | Annot.BuiltinMul builtin -> norm Ir.builtin_mul env builtin.ty kontinue
  | Annot.BuiltinDiv builtin -> norm Ir.builtin_div env builtin.ty kontinue
  | Annot.BuiltinMod builtin -> norm Ir.builtin_mod env builtin.ty kontinue
  | Annot.BuiltinExp builtin -> norm Ir.builtin_exp env builtin.ty kontinue
  | Annot.BuiltinNeg builtin -> norm Ir.builtin_neg env builtin.ty kontinue
  | Annot.BuiltinBitAnd builtin -> norm Ir.builtin_bit_and env builtin.ty kontinue
  | Annot.BuiltinBitOr builtin -> norm Ir.builtin_bit_or env builtin.ty kontinue
  | Annot.BuiltinBitNot builtin -> norm Ir.builtin_bit_not env builtin.ty kontinue
  | Annot.BuiltinBitXor builtin -> norm Ir.builtin_bit_xor env builtin.ty kontinue
  | Annot.BuiltinLogNot -> kontinue Ir.builtin_log_not
  | Annot.BuiltinPromote builtin -> norm_builtin_promote env builtin.sub builtin.sup kontinue
  | Annot.BuiltinConcat builtin -> norm Ir.builtin_concat env builtin.ty kontinue

and norm_builtin_promote env sub sup kontinue =
  Type.norm_ty env sub (fun sub ->
    Type.norm_ty env sup (fun sup ->
      Ir.builtin_promote sub sup
        |> kontinue))
