(* Abstract Syntax *)

open Common

type atom =
  | Bool of { value: bool }
  | Int of { value: int32 }
  | Long of { value: int64 }
  | Float of { value: float }
  | Double of { value: float }
  | Rune of { value: Uchar.t }
  | String of { value: string; len: int }
  | Ident of { id: Sym.t }

type expr =
  | Builtin of { fn: Builtin.builtin; args: atom list }
  | Atom of { atom: atom }

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

type binding =
  | Binding of { patt: patt; ty: Type.ty; value: expr }

type block =
  | Bind of { binding: binding; scope: block }
  | Expr of { expr: expr }

type top =
  | Let of { binding: binding }

let atom_bool value = Bool { value }
let atom_int value = Int { value }
let atom_long value = Long { value }
let atom_float value = Float { value }
let atom_double value = Double { value }
let atom_rune value = Rune { value }
let atom_string value =
  let value = Utf8.normalize value in
  String { value; len = Utf8.length value }
let atom_ident id = Ident { id }

let expr_builtin fn args = Builtin { fn; args }
let expr_atom atom = Atom { atom }

let patt_ground = PattGround
let patt_var id = PattVar { id }

let binding patt ty value = Binding { patt; ty; value }

let block_bind binding scope = Bind { binding; scope }
let block_expr expr = Expr { expr }

let top_let binding = Let { binding }
