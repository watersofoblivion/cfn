(* Operators *)

(* Promotion *)

let integral_rule lhs rhs = match (lhs, rhs) with
  | Annot.TyInt, Annot.TyInt -> Some (None, None)
  | Annot.TyLong, Annot.TyLong -> Some (None, None)
  | Annot.TyInt, Annot.TyLong -> Some (Some Annot.ty_long, None)
  | Annot.TyLong, Annot.TyInt -> Some (None, Some Annot.ty_long)
  | _ -> None

let floating_point_rule lhs rhs = match (lhs, rhs) with
  | Annot.TyFloat, Annot.TyFloat -> Some (None, None)
  | Annot.TyDouble, Annot.TyDouble -> Some (None, None)
  | Annot.TyFloat, Annot.TyDouble -> Some (Some Annot.ty_double, None)
  | Annot.TyDouble, Annot.TyFloat -> Some (None, Some Annot.ty_double)
  | _ -> None

let numeric_rule lhs rhs =
  match integral_rule lhs rhs with
    | Some rule -> Some rule
    | None ->
      match floating_point_rule lhs rhs with
        | Some rule -> Some rule
        | None ->
          match (lhs, rhs) with
            | Annot.TyInt, Annot.TyDouble -> Some (Some Annot.ty_double, None)
            | Annot.TyDouble, Annot.TyInt -> Some (None, Some Annot.ty_double)
            | _ -> None

let eq_rule _ _ = Some (None, None)
let cmp_rule = numeric_rule

let promotion_rule = function
  | Syntax.BinStructEq _
  | Syntax.BinStructNeq _
  | Syntax.BinPhysEq _
  | Syntax.BinPhysNeq _ -> eq_rule
  | Syntax.BinLt _
  | Syntax.BinLte _
  | Syntax.BinGt _
  | Syntax.BinGte _ -> cmp_rule
  | Syntax.BinAdd _
  | Syntax.BinSub _
  | Syntax.BinMul _
  | Syntax.BinDiv _ -> numeric_rule
  | Syntax.BinMod _ -> integral_rule
  | Syntax.BinExp _ -> integral_rule
  | Syntax.BinLogAnd _
  | Syntax.BinLogOr _ -> failwith "TODO"
  | Syntax.BinBitAnd _
  | Syntax.BinBitOr _
  | Syntax.BinBitXor _ -> integral_rule

(* Unary *)

let rec desug_un env op operand kontinue = match op with
  | Syntax.UnNeg _ -> desug_un_neg env operand kontinue
  | Syntax.UnBitNot _ -> desug_un_bit_not env operand kontinue
  | Syntax.UnLogNot _ -> desug_un_log_not env operand kontinue

and desug_un_neg _ ty kontinue =
  if Annot.ty_is_numeric ty
  then
    Annot.builtin_neg ty
      |> kontinue ty
  else failwith "TODO"

and desug_un_bit_not _ ty kontinue =
  if Annot.ty_is_integral ty
  then
    Annot.builtin_bit_not ty
      |> kontinue ty
  else failwith "TODO"

and desug_un_log_not _ ty kontinue =
  if Annot.ty_is_logical ty
  then kontinue ty Annot.builtin_log_not
  else failwith "TODO"

(* Binary *)

let desug_bin_op _ constr test lhs rhs kontinue =
  if Annot.ty_equal lhs rhs
  then
    if test lhs
    then
      constr lhs
        |> kontinue lhs
    else failwith "TODO"
  else failwith "TODO"

let rec desug_bin env op lhs rhs kontinue = match op with
  | Syntax.BinStructEq _ -> desug_bin_eq env Annot.builtin_struct_eq lhs rhs kontinue
  | Syntax.BinStructNeq _ -> desug_bin_eq env Annot.builtin_struct_neq lhs rhs kontinue
  | Syntax.BinPhysEq _ -> desug_bin_eq env Annot.builtin_phys_eq lhs rhs kontinue
  | Syntax.BinPhysNeq _ -> desug_bin_eq env Annot.builtin_phys_neq lhs rhs kontinue
  | Syntax.BinLt _ -> desug_bin_cmp env Annot.builtin_lt lhs rhs kontinue
  | Syntax.BinLte _ -> desug_bin_cmp env Annot.builtin_lte lhs rhs kontinue
  | Syntax.BinGt _ -> desug_bin_cmp env Annot.builtin_gt lhs rhs kontinue
  | Syntax.BinGte _ -> desug_bin_cmp env Annot.builtin_gte lhs rhs kontinue
  | Syntax.BinAdd _ -> desug_bin_numeric env Annot.builtin_add lhs rhs kontinue
  | Syntax.BinSub _ -> desug_bin_numeric env Annot.builtin_sub lhs rhs kontinue
  | Syntax.BinMul _ -> desug_bin_numeric env Annot.builtin_mul lhs rhs kontinue
  | Syntax.BinDiv _ -> desug_bin_numeric env Annot.builtin_div lhs rhs kontinue
  | Syntax.BinMod _ -> desug_bin_integral env Annot.builtin_mod lhs rhs kontinue
  | Syntax.BinExp _ -> desug_bin_floating_point env Annot.builtin_exp lhs rhs kontinue
  | Syntax.BinLogAnd _ -> desug_bin_log env () lhs rhs kontinue
  | Syntax.BinLogOr _ -> desug_bin_log env () lhs rhs kontinue
  | Syntax.BinBitAnd _ -> desug_bin_integral env Annot.builtin_bit_and lhs rhs kontinue
  | Syntax.BinBitOr _ -> desug_bin_integral env Annot.builtin_bit_or lhs rhs kontinue
  | Syntax.BinBitXor _ -> desug_bin_integral env Annot.builtin_bit_xor lhs rhs kontinue

and desug_bin_eq _ constr lhs rhs kontinue =
  if Annot.ty_equal lhs rhs
  then
    constr lhs
      |> kontinue Annot.ty_bool
  else failwith "TODO"

and desug_bin_cmp _ constr lhs rhs kontinue =
  if Annot.ty_equal lhs rhs
  then
    if Annot.ty_is_numeric lhs
    then
      constr lhs
        |> kontinue Annot.ty_bool
    else failwith "TODO"
  else failwith "TODO"

and desug_bin_numeric env constr lhs rhs kontinue =
  desug_bin_op env constr Annot.ty_is_numeric lhs rhs kontinue

and desug_bin_integral env constr lhs rhs kontinue =
  desug_bin_op env constr Annot.ty_is_floating_point lhs rhs kontinue

and desug_bin_floating_point env constr lhs rhs kontinue =
  desug_bin_op env constr Annot.ty_is_integral lhs rhs kontinue

and desug_bin_log _ constr lhs rhs kontinue =
  let _ = constr in
  let _ = lhs in
  let _ = rhs in
  let _ = kontinue in
  failwith "TODO"
