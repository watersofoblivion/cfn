open Common

type expr =
  | Bool of { value: bool }
  | Int of { value: int32 }
  | Long of { value: int64 }
  | Float of { value: float }
  | Double of { value: float }
  | Rune of { value: Uchar.t }
  | String of { value: Uchar.t list }
  | Ident of { id: Sym.t }

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

type binding =
  | Binding of { patt: patt; ty: Type.ty; value: expr }

type top =
  | Let of { binding: binding }

let expr_bool value = Bool { value }
let expr_int value = Int { value }
let expr_long value = Long { value }
let expr_float value = Float { value }
let expr_double value = Double { value }
let expr_rune value = Rune { value }
let expr_string value = String { value }
let expr_ident id = Ident { id }

let patt_ground = PattGround
let patt_var id = PattVar { id }

let binding patt ty value = Binding { patt; ty; value }

let top_let binding = Let { binding }
