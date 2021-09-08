(* Types *)

let convert_ty _ ty kontinue = match ty with
  | Mono.TyBool -> kontinue Clos.ty_bool
  | Mono.TyInt -> kontinue Clos.ty_int
  | Mono.TyLong -> kontinue Clos.ty_long
  | Mono.TyFloat -> kontinue Clos.ty_float
  | Mono.TyDouble -> kontinue Clos.ty_double
  | Mono.TyRune -> kontinue Clos.ty_rune
  | Mono.TyString -> kontinue Clos.ty_string
