(* Atomic Values *)

open Format

open Common

(* Syntax *)

type atom =
  | AtomBool of { value: bool }
  | AtomInt of { value: int32 }
  | AtomLong of { value: int64 }
  | AtomFloat of { value: float }
  | AtomDouble of { value: float }
  | AtomRune of { value: Uchar.t }
  | AtomString of { value: string; len: int }
  | AtomIdent of { id: Sym.t }

(* Constructors *)

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

(* Pretty Printing *)

let pp_atom fmt = function
  | AtomBool atom -> fprintf fmt "%B" atom.value
  | AtomInt atom -> fprintf fmt "%ld" atom.value
  | AtomLong atom -> fprintf fmt "%Ld" atom.value
  | AtomFloat atom -> fprintf fmt "%g" atom.value
  | AtomDouble atom -> fprintf fmt "%g" atom.value
  | AtomRune atom ->
    atom.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | AtomString atom -> fprintf fmt "%S" atom.value
  | AtomIdent atom -> Sym.pp fmt atom.id

(* Type Checking *)

let rec check_atom env atom kontinue = match atom with
  | AtomBool _ -> kontinue Type.ty_bool
  | AtomInt _ -> kontinue Type.ty_int
  | AtomLong _ -> kontinue Type.ty_long
  | AtomFloat _ -> kontinue Type.ty_float
  | AtomDouble _ -> kontinue Type.ty_double
  | AtomRune _ -> kontinue Type.ty_rune
  | AtomString _ -> kontinue Type.ty_string
  | AtomIdent atom -> check_atom_ident env atom.id kontinue

and check_atom_ident env id kontinue =
  try
    Env.lookup id env
      |> kontinue
  with Not_found -> Check.unbound_identifier id
