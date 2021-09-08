(* Types *)

let convert_ty _ ty kontinue = match ty with
  | Ir.TyBool -> kontinue Mono.ty_bool
  | Ir.TyInt -> kontinue Mono.ty_int
  | Ir.TyLong -> kontinue Mono.ty_long
  | Ir.TyFloat -> kontinue Mono.ty_float
  | Ir.TyDouble -> kontinue Mono.ty_double
  | Ir.TyRune -> kontinue Mono.ty_rune
  | Ir.TyString -> kontinue Mono.ty_string
