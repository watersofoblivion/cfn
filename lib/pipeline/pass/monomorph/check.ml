open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Mono.ty * Mono.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise
