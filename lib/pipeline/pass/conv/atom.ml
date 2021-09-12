(* Atomic Values *)

open Common

let convert_atom_value constr ty value kontinue =
  value
    |> constr
    |> kontinue ty

let rec convert_atom env atom kontinue = match atom with
  | Mono.AtomBool atom -> convert_atom_value Clos.atom_bool Clos.ty_bool atom.value kontinue
  | Mono.AtomInt atom -> convert_atom_value Clos.atom_int Clos.ty_int atom.value kontinue
  | Mono.AtomLong atom -> convert_atom_value Clos.atom_long Clos.ty_long atom.value kontinue
  | Mono.AtomFloat atom -> convert_atom_value Clos.atom_float Clos.ty_float atom.value kontinue
  | Mono.AtomDouble atom -> convert_atom_value Clos.atom_double Clos.ty_double atom.value kontinue
  | Mono.AtomRune atom -> convert_atom_value Clos.atom_rune Clos.ty_rune atom.value kontinue
  | Mono.AtomString atom -> convert_atom_value Clos.atom_string Clos.ty_string atom.value kontinue
  | Mono.AtomIdent atom -> convert_atom_ident env atom.id kontinue

and convert_atom_ident env id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Clos.atom_ident
      |> kontinue ty
  with Not_found -> Check.unbound_identifier id
