(* Built-in Functions *)

open Format

(* Syntax *)

type arity =
  | ArityFixed of { arity: int; args: Type.ty list; res: Type.ty  }
  | ArityVar of { args: Type.ty; res: Type.ty }

type builtin =
  | BuiltinStructEq of { ty: Type.ty }
  | BuiltinStructNeq of { ty: Type.ty }
  | BuiltinPhysEq of { ty: Type.ty }
  | BuiltinPhysNeq of { ty: Type.ty }
  | BuiltinLt of { ty: Type.ty }
  | BuiltinLte of { ty: Type.ty }
  | BuiltinGt of { ty: Type.ty }
  | BuiltinGte of { ty: Type.ty }
  | BuiltinAdd of { ty: Type.ty }
  | BuiltinSub of { ty: Type.ty }
  | BuiltinMul of { ty: Type.ty }
  | BuiltinDiv of { ty: Type.ty }
  | BuiltinMod of { ty: Type.ty }
  | BuiltinExp of { ty: Type.ty }
  | BuiltinNeg of { ty: Type.ty }
  | BuiltinBitAnd of { ty: Type.ty }
  | BuiltinBitOr of { ty: Type.ty }
  | BuiltinBitNot of { ty: Type.ty }
  | BuiltinBitXor of { ty: Type.ty }
  | BuiltinLogNot
  | BuiltinPromote of { sub: Type.ty; sup: Type.ty }
  | BuiltinConcat of { ty: Type.ty }

(* Exceptions *)

exception UnsupportedConcatType of Type.ty
exception NotIntegral of Type.ty
exception NotFloatingPoint of Type.ty
exception NotNumeric of Type.ty
exception UnsupportedPromotion of Type.ty * Type.ty

let unsupported_concat_type ty =
  UnsupportedConcatType ty
    |> raise

let not_integral ty =
  NotIntegral ty
    |> raise

let not_floating_point ty =
  NotFloatingPoint ty
    |> raise

let not_numeric ty =
  NotNumeric ty
    |> raise

let unsupported_promotion sub sup =
  UnsupportedPromotion (sub, sup)
    |> raise

(* Constructors *)

let arity_fixed args res = ArityFixed { arity = List.length args; args; res }
let arity_var args res = ArityVar { args; res }

let builtin test error constr ty =
  if test ty
  then constr ty
  else error ty

let builtin_integral = builtin Type.ty_is_integral not_integral
let builtin_floating_point = builtin Type.ty_is_floating_point not_floating_point
let builtin_numeric = builtin Type.ty_is_numeric not_numeric

let builtin_struct_eq ty = BuiltinStructEq { ty }
let builtin_struct_neq ty = BuiltinStructNeq { ty }
let builtin_phys_eq ty = BuiltinPhysEq { ty }
let builtin_phys_neq ty = BuiltinPhysNeq { ty }

let builtin_lt = builtin_numeric (fun ty -> BuiltinLt { ty })
let builtin_lte = builtin_numeric (fun ty -> BuiltinLte { ty })
let builtin_gt = builtin_numeric (fun ty -> BuiltinGt { ty })
let builtin_gte = builtin_numeric (fun ty -> BuiltinGte { ty })

let builtin_add = builtin_numeric (fun ty -> BuiltinAdd { ty })
let builtin_sub = builtin_numeric (fun ty -> BuiltinSub { ty })
let builtin_mul = builtin_numeric (fun ty -> BuiltinMul { ty })
let builtin_div = builtin_numeric (fun ty -> BuiltinDiv { ty })
let builtin_mod = builtin_integral (fun ty -> BuiltinMod { ty })
let builtin_exp = builtin_floating_point (fun ty -> BuiltinExp { ty })
let builtin_neg = builtin_numeric (fun ty -> BuiltinNeg { ty })

let builtin_bit_and = builtin_integral (fun ty -> BuiltinBitAnd { ty })
let builtin_bit_or = builtin_integral (fun ty -> BuiltinBitOr { ty })
let builtin_bit_not = builtin_integral (fun ty -> BuiltinBitNot { ty })
let builtin_bit_xor = builtin_integral (fun ty -> BuiltinBitXor { ty })

let builtin_log_not = BuiltinLogNot

let builtin_promote sub sup = match (sub, sup) with
  | Type.TyInt, Type.TyLong
  | Type.TyInt, Type.TyDouble
  | Type.TyFloat, Type.TyFloat -> BuiltinPromote { sub; sup }
  | _ -> unsupported_promotion sub sup

let builtin_concat ty = match ty with
  | Type.TyString -> BuiltinConcat { ty }
  | _ -> unsupported_concat_type ty

(* Pretty Printing *)

let rec pp_arity fmt = function
  | ArityFixed arity -> pp_arity_fixed fmt arity.args arity.res
  | ArityVar arity -> pp_arity_var fmt arity.args arity.res

and pp_arity_fixed fmt args res =
  let pp_sep fmt _ = fprintf fmt ", " in
  fprintf fmt "(%a): %a" (pp_print_list ~pp_sep Type.pp_ty) args Type.pp_ty res

and pp_arity_var fmt args res =
  fprintf fmt "(%a...): %a" Type.pp_ty args Type.pp_ty res

let pp_builtin fmt builtin =
  let pp id tys =
    let pp_sep fmt _ = fprintf fmt ", " in
    match tys with
      | [] -> fprintf fmt "%s" id
      | _ -> fprintf fmt "%s[%a]" id (pp_print_list ~pp_sep Type.pp_ty) tys
  in
  match builtin with
    | BuiltinStructEq builtin -> pp "structEq" [builtin.ty]
    | BuiltinStructNeq builtin -> pp "structNeq" [builtin.ty]
    | BuiltinPhysEq builtin -> pp "physEq" [builtin.ty]
    | BuiltinPhysNeq builtin -> pp "physNeq" [builtin.ty]
    | BuiltinLt builtin -> pp "lt" [builtin.ty]
    | BuiltinLte builtin -> pp "lte" [builtin.ty]
    | BuiltinGt builtin -> pp "gt" [builtin.ty]
    | BuiltinGte builtin -> pp "gte" [builtin.ty]
    | BuiltinAdd builtin -> pp "add" [builtin.ty]
    | BuiltinSub builtin -> pp "sub" [builtin.ty]
    | BuiltinMul builtin -> pp "mul" [builtin.ty]
    | BuiltinDiv builtin -> pp "div" [builtin.ty]
    | BuiltinMod builtin -> pp "mod" [builtin.ty]
    | BuiltinExp builtin -> pp "exp" [builtin.ty]
    | BuiltinNeg builtin -> pp "neg" [builtin.ty]
    | BuiltinBitAnd builtin -> pp "bitAnd" [builtin.ty]
    | BuiltinBitOr builtin -> pp "bitOr" [builtin.ty]
    | BuiltinBitNot builtin -> pp "bitNot" [builtin.ty]
    | BuiltinBitXor builtin -> pp "bitXor" [builtin.ty]
    | BuiltinLogNot -> pp "logNot" []
    | BuiltinPromote builtin -> pp "promote" [builtin.sub; builtin.sup]
    | BuiltinConcat builtin -> pp "concat" [builtin.ty]

(* Type Checking *)

let rec check_builtin env builtin kontinue = match builtin with
  | BuiltinStructEq builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinStructNeq builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinPhysEq builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinPhysNeq builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinLt builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinLte builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinGt builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinGte builtin -> check_builtin_cmp env builtin.ty kontinue
  | BuiltinAdd builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinSub builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinMul builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinDiv builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinMod builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinExp builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinNeg builtin -> check_builtin_un_op env builtin.ty kontinue
  | BuiltinBitAnd builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinBitOr builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinBitNot builtin -> check_builtin_un_op env builtin.ty kontinue
  | BuiltinBitXor builtin -> check_builtin_bin_op env builtin.ty kontinue
  | BuiltinLogNot -> check_builtin_un_op env Type.ty_bool kontinue
  | BuiltinPromote builtin -> check_builtin_promote env builtin.sub builtin.sup kontinue
  | BuiltinConcat builtin -> check_builtin_concat env builtin.ty kontinue

and check_builtin_cmp _ ty kontinue =
  arity_fixed [ty; ty] Type.ty_bool
    |> kontinue

and check_builtin_un_op _ ty kontinue =
  arity_fixed [ty] ty
    |> kontinue

and check_builtin_bin_op _ ty kontinue =
  arity_fixed [ty; ty] ty
    |> kontinue

and check_builtin_promote _ sub sup kontinue =
  arity_fixed [sub] sup
    |> kontinue

and check_builtin_concat _ ty kontinue =
  arity_var ty ty
    |> kontinue
