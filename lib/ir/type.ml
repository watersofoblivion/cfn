(** {1 Types} *)

type t =
  | Bool
  | Int
  | Long
  | Float
  | Double
  | Rune
  | String

(* Constructors *)

let bool = Bool
let int = Int
let long = Long
let float = Float
let double = Double
let rune = Rune
let string = String

let equal ty ty' = match (ty, ty') with
  | Bool, Bool
  | Int, Int
  | Long, Long
  | Float, Float
  | Double, Double
  | Rune, Rune
  | String, String -> true
  | _ -> false
