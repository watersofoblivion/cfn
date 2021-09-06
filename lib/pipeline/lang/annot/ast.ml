(* Abstract Syntax *)

open Common

(* Syntax *)

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

type expr =
  | ExprBool of { value: bool }
  | ExprInt of { value: int32 }
  | ExprLong of { value: int64 }
  | ExprFloat of { value: float }
  | ExprDouble of { value: float }
  | ExprRune of { value: Uchar.t }
  | ExprString of { value: string; len: int }
  | ExprIdent of { id: Sym.t }
  | ExprBuiltin of { fn: Builtin.builtin; args: expr list }

and binding =
  | Binding of { patt: patt; ty: Type.ty; value: expr }

type top =
  | TopLet of { binding: binding }

(* Patterns *)

let patt_ground = PattGround
let patt_var id = PattVar { id }

(* Expressions *)

let expr_bool value = ExprBool { value }
let expr_int value = ExprInt { value }
let expr_long value = ExprLong { value }
let expr_float value = ExprFloat { value }
let expr_double value = ExprDouble { value }
let expr_rune value = ExprRune { value }
let expr_string value =
  let value = Utf8.normalize value in
  ExprString { value; len = Utf8.length value }
let expr_ident id = ExprIdent { id }
let expr_builtin fn args = ExprBuiltin { fn; args }

(* Bindings *)

let binding patt ty value = Binding { patt; ty; value }

(* Top-Level Expressions *)

let top_let binding = TopLet { binding }
