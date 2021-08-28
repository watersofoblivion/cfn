(** {1 Types} *)

type ty =
  | TyBool
  | TyInt
  | TyLong
  | TyFloat
  | TyDouble
  | TyRune
  | TyString

(* Constructors *)

let ty_bool = TyBool
let ty_int = TyInt
let ty_long = TyLong
let ty_float = TyFloat
let ty_double = TyDouble
let ty_rune = TyRune
let ty_string = TyString

let ty_equal ty ty' = match (ty, ty') with
  | TyBool, TyBool
  | TyInt, TyInt
  | TyLong, TyLong
  | TyFloat, TyFloat
  | TyDouble, TyDouble
  | TyRune, TyRune
  | TyString, TyString -> true
  | _ -> false
