open Common

exception UnboundIdentifier of { id: Sym.t }
exception MismatchedTypes of { inferred: Ir.ty; annotated: Ir.ty }

let unbound_identifier id =
  UnboundIdentifier { id }
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes { inferred; annotated }
    |> raise
