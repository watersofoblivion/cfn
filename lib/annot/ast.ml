type expr =
  | Bool of { value: bool }
  | Int of { value: int32 }
  | Long of { value: int64 }
  | Float of { value: float }
  | Double of { value: float }
  | Rune of { value: Uchar.t }
  | String of { value: Uchar.t list }

let bool value = Bool { value }
let int value = Int { value }
let long value = Long { value }
let float value = Float { value }
let double value = Double { value }
let rune value = Rune { value }
let string value = String { value }
