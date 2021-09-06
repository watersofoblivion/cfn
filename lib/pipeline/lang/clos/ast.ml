(* Abstract Syntax *)

open Common

type atom =
  | AtomBool of { value: bool }
  | AtomInt of { value: int32 }
  | AtomLong of { value: int64 }
  | AtomFloat of { value: float }
  | AtomDouble of { value: float }
  | AtomRune of { value: Uchar.t }
  | AtomString of { value: string; len: int }
  | AtomIdent of { id: Sym.t }

type expr =
  | ExprBuiltin of { fn: Builtin.builtin; args: atom list }
  | ExprAtom of { atom: atom }

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

type binding =
  | Binding of { patt: patt; ty: Type.ty; value: expr }

type block =
  | BlockLet of { binding: binding; scope: block }
  | BlockExpr of { expr: expr }

type top =
  | TopLet of { binding: binding }

let atom_bool value = AtomBool { value }
let atom_int value = AtomInt { value }
let atom_long value = AtomLong { value }
let atom_float value = AtomFloat { value }
let atom_double value = AtomDouble { value }
let atom_rune value = AtomRune { value }
let atom_string value =
  let value = Utf8.normalize value in
  AtomString { value; len = Utf8.length value }
let atom_ident id = AtomIdent { id }

let expr_builtin fn args = ExprBuiltin { fn; args }
let expr_atom atom = ExprAtom { atom }

let patt_ground = PattGround
let patt_var id = PattVar { id }

let binding patt ty value = Binding { patt; ty; value }

let block_let binding scope = BlockLet { binding; scope }
let block_expr expr = BlockExpr { expr }

let top_let binding = TopLet { binding }
