open Common

type atom =
  | Bool of { value: bool }
  | Int of { value: int32 }
  | Long of { value: int64 }
  | Float of { value: float }
  | Double of { value: float }
  | Rune of { value: Uchar.t }
  | String of { value: Uchar.t list }
  | Ident of { id: Sym.t }

type expr =
  | Atom of { atom: atom }

type block =
  | Expr of { expr: expr }

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

type binding =
  | Binding of { patt: patt; ty: Type.t; value: expr }

type top =
  | Let of { binding: binding }

let atom_bool value = Bool { value }
let atom_int value = Int { value }
let atom_long value = Long { value }
let atom_float value = Float { value }
let atom_double value = Double { value }
let atom_rune value = Rune { value }
let atom_string value = String { value }
let atom_ident id = Ident { id }

let expr_atom atom = Atom { atom }

let block_expr expr = Expr { expr }

let patt_ground = PattGround
let patt_var id = PattVar { id }

let binding patt ty value = Binding { patt; ty; value }

let top_let binding = Let { binding }
