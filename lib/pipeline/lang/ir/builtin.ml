type builtin =
  | Add of { ty: Type.ty }
  | Sub of { ty: Type.ty }
  | Mul of { ty: Type.ty }
  | Div of { ty: Type.ty }
  | Mod of { ty: Type.ty }
  | Exp of { ty: Type.ty }
  | Promote of { sub: Type.ty; sup: Type.ty }
  | Concat of { ty: Type.ty }

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

let builtin test error constr ty =
  if test ty
  then constr ty
  else error ty

let builtin_integral = builtin Type.ty_is_integral not_integral
let builtin_floating_point = builtin Type.ty_is_floating_point not_floating_point
let builtin_numeric = builtin Type.ty_is_numeric not_numeric

let builtin_add = builtin_numeric (fun ty -> Add { ty })
let builtin_sub = builtin_numeric (fun ty -> Sub { ty })
let builtin_mul = builtin_numeric (fun ty -> Mul { ty })
let builtin_div = builtin_numeric (fun ty -> Div { ty })
let builtin_mod = builtin_integral (fun ty -> Mod { ty })
let builtin_exp = builtin_floating_point (fun ty -> Exp { ty })

let builtin_promote sub sup = match (sub, sup) with
  | Type.TyInt, Type.TyLong
  | Type.TyInt, Type.TyDouble
  | Type.TyFloat, Type.TyFloat -> Promote { sub; sup }
  | _ -> unsupported_promotion sub sup

let builtin_concat ty = match ty with
  | Type.TyString -> Concat { ty }
  | _ -> unsupported_concat_type ty
