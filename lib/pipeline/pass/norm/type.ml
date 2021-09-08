(* Types *)

let norm_ty _ ty kontinue = match ty with
  | Annot.TyBool -> kontinue Ir.ty_bool
  | Annot.TyInt -> kontinue Ir.ty_int
  | Annot.TyLong -> kontinue Ir.ty_long
  | Annot.TyFloat -> kontinue Ir.ty_float
  | Annot.TyDouble -> kontinue Ir.ty_double
  | Annot.TyRune -> kontinue Ir.ty_rune
  | Annot.TyString -> kontinue Ir.ty_string
