type atom =
  | Bool of { value: bool }
  | Int of { value: int32 }
  | Long of { value: int64 }
  | Float of { value: float }
  | Double of { value: float }
  | Rune of { value: Uchar.t }
  | String of { value: Uchar.t list }

type expr =
  | Atom of { atom: atom }

type block =
  | Expr of { expr: expr }

let atom_bool value = Bool { value }
let atom_int value = Int { value }
let atom_long value = Long { value }
let atom_float value = Float { value }
let atom_double value = Double { value }
let atom_rune value = Rune { value }
let atom_string value = String { value }

let expr_atom atom = Atom { atom }

let block_expr expr = Expr { expr }
