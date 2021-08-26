(** {1 Type Checking} *)

open Common

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Type.t * Type.t

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let check_expr env expr kontinue = match expr with
  | Ast.Bool _ -> kontinue Type.bool
  | Ast.Int _ -> kontinue Type.int
  | Ast.Long _ -> kontinue Type.long
  | Ast.Float _ -> kontinue Type.float
  | Ast.Double _ -> kontinue Type.double
  | Ast.Rune _ -> kontinue Type.rune
  | Ast.String _ -> kontinue Type.string
  | Ast.Ident ident ->
    try
      Env.lookup ident.id env
        |> kontinue
    with Not_found -> unbound_identifier ident.id

let check_patt env patt ty kontinue = match patt with
  | Ast.PattGround -> kontinue env
  | Ast.PattVar patt -> Env.bind patt.id ty env kontinue

let check_binding env binding kontinue = match binding with
  | Ast.Binding binding ->
    check_expr env binding.value (fun expr ->
      if Type.equal binding.ty expr
      then check_patt env binding.patt expr kontinue
      else mismatched_types expr binding.ty)

let check_top env top kontinue = match top with
  | Ast.Let top -> check_binding env top.binding kontinue
