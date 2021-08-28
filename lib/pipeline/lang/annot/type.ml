(** {1 Types} *)

type ty =
  | Bool
  | Int
  | Long
  | Float
  | Double
  | Rune
  | String

(* Constructors *)

let ty_bool = Bool
let ty_int = Int
let ty_long = Long
let ty_float = Float
let ty_double = Double
let ty_rune = Rune
let ty_string = String

let ty_equal ty ty' = match (ty, ty') with
  | Bool, Bool
  | Int, Int
  | Long, Long
  | Float, Float
  | Double, Double
  | Rune, Rune
  | String, String -> true
  | _ -> false
