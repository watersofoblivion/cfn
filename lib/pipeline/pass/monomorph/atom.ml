open Common

let convert_atom_value constr ty value kontinue =
  value
    |> constr
    |> kontinue ty

let rec convert_atom env atom kontinue = match atom with
  | Ir.AtomBool atom -> convert_atom_value Mono.atom_bool Mono.ty_bool atom.value kontinue
  | Ir.AtomInt atom -> convert_atom_value Mono.atom_int Mono.ty_int atom.value kontinue
  | Ir.AtomLong atom -> convert_atom_value Mono.atom_long Mono.ty_long atom.value kontinue
  | Ir.AtomFloat atom -> convert_atom_value Mono.atom_float Mono.ty_float atom.value kontinue
  | Ir.AtomDouble atom -> convert_atom_value Mono.atom_double Mono.ty_double atom.value kontinue
  | Ir.AtomRune atom -> convert_atom_value Mono.atom_rune Mono.ty_rune atom.value kontinue
  | Ir.AtomString atom -> convert_atom_value Mono.atom_string Mono.ty_string atom.value kontinue
  | Ir.AtomIdent atom -> convert_atom_ident env atom.id kontinue

and convert_atom_ident env id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Mono.atom_ident
      |> kontinue ty
  with Not_found -> Check.unbound_identifier id
