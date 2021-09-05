open Common

(* Syntax *)

type patt =
  | PattGround
  | PattVar of { id: Sym.t }

type expr =
  | Bool of { value: bool }
  | Int of { value: int32 }
  | Long of { value: int64 }
  | Float of { value: float }
  | Double of { value: float }
  | Rune of { value: Uchar.t }
  | String of { value: string; len: int }
  | Ident of { id: Sym.t }
  | Builtin of { fn: Builtin.builtin; args: expr list }

and binding =
  | Binding of { patt: patt; ty: Type.ty; value: expr }

type top =
  | Let of { binding: binding }

(* Patterns *)

let patt_ground = PattGround
let patt_var id = PattVar { id }

(* Expressions *)

let expr_bool value = Bool { value }
let expr_int value = Int { value }
let expr_long value = Long { value }
let expr_float value = Float { value }
let expr_double value = Double { value }
let expr_rune value = Rune { value }
let expr_string value =
  let value = Utf8.normalize value in
  String { value; len = Utf8.length value }
let expr_ident id = Ident { id }
let expr_builtin fn args = Builtin { fn; args }

(* Bindings *)

let binding patt ty value = Binding { patt; ty; value }

(* Top-Level Expressions *)

let top_let binding = Let { binding }
