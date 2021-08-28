open Common

exception InvalidNumberFormat of Loc.t * string * Sym.t * string
exception UnboundConstructor of Loc.t * Sym.t
exception UnboundIdentifier of Loc.t * Sym.t
exception MismatchedTypes of Annot.Type.t * Loc.t * Annot.Type.t

let invalid_number_format loc lexeme constr msg =
  InvalidNumberFormat (loc, lexeme, constr, msg)
    |> raise

let unbound_constructor loc id =
  UnboundConstructor (loc, id)
    |> raise

let unbound_identifier loc id =
  UnboundIdentifier (loc, id)
    |> raise

let mismatched_types inferred loc annotated =
  MismatchedTypes (inferred, loc, annotated)
    |> raise

let with_constructors env kontinue =
  let constrs = [
    (Prim.id_bool, Annot.Type.bool);
    (Prim.id_int, Annot.Type.int);
    (Prim.id_long, Annot.Type.long);
    (Prim.id_float, Annot.Type.float);
    (Prim.id_double, Annot.Type.double);
    (Prim.id_rune, Annot.Type.rune);
    (Prim.id_string, Annot.Type.string);
  ] in
  let fold env constr = Env.constr_of constr env (fun env _ -> env) in
  let env =
    constrs
      |> List.map fst
      |> List.fold_left fold env
  in
  let map (constr, annot) = Env.constr_of constr env (fun _ sym -> (sym, annot)) in
  List.map map constrs
    |> kontinue

let desug_ty env ty kontinue =
  with_constructors env (fun mappings ->
    match ty with
      | Syntax.Type.Constr constr ->
        try
          List.assoc constr.id mappings
            |> kontinue
        with Not_found -> unbound_constructor constr.loc constr.id)

let number_suffix = Str.regexp "[iIlLfFdD]$"
let normalize_number constr conv env loc lexeme =
  try
    lexeme
      |> Str.global_replace number_suffix ""
      |> String.lowercase_ascii
      |> conv
  with Failure msg ->
    Env.constr_of constr env (fun _ sym ->
      invalid_number_format loc lexeme sym msg)

let normalize_int = normalize_number Prim.id_int Int32.of_string
let normalize_long = normalize_number Prim.id_long Int64.of_string
let normalize_float = normalize_number Prim.id_float Float.of_string
let normalize_double = normalize_number Prim.id_double Float.of_string

let desug_expr env expr kontinue = match expr with
  | Syntax.Ast.Bool expr ->
    expr.value
      |> Annot.Ast.bool
      |> kontinue Annot.Type.bool
  | Syntax.Ast.Int expr ->
    expr.lexeme
      |> normalize_int env expr.loc
      |> Annot.Ast.int
      |> kontinue Annot.Type.int
  | Syntax.Ast.Long expr ->
    expr.lexeme
      |> normalize_long env expr.loc
      |> Annot.Ast.long
      |> kontinue Annot.Type.long
  | Syntax.Ast.Float expr ->
    expr.lexeme
      |> normalize_float env expr.loc
      |> Annot.Ast.float
      |> kontinue Annot.Type.float
  | Syntax.Ast.Double expr ->
    expr.lexeme
      |> normalize_double env expr.loc
      |> Annot.Ast.double
      |> kontinue Annot.Type.double
  | Syntax.Ast.Rune expr ->
    expr.value
      |> Annot.Ast.rune
      |> kontinue Annot.Type.rune
  | Syntax.Ast.String expr ->
    expr.value
      |> Annot.Ast.string
      |> kontinue Annot.Type.string
  | Syntax.Ast.Ident expr ->
    try
      let ty = Env.lookup expr.id env in
      expr.id
        |> Annot.Ast.ident
        |> kontinue ty
    with Not_found -> unbound_identifier expr.loc expr.id

let desug_patt env patt ty kontinue = match patt with
  | Syntax.Ast.PattGround _ ->
    Annot.Ast.patt_ground
      |> kontinue env
  | Syntax.Ast.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      Annot.Ast.patt_var patt.id
        |> kontinue env)

let desug_binding env binding kontinue = match binding with
  | Syntax.Ast.ValueBinding binding ->
    desug_expr env binding.value (fun inferred value ->
      match binding.ty with
        | Some ty ->
          desug_ty env ty (fun annotated ->
            if Annot.Type.equal inferred annotated
            then
              desug_patt env binding.patt inferred (fun env patt ->
                Annot.Ast.binding patt inferred value
                  |> kontinue env)
            else
              let loc = Syntax.Type.loc_ty ty in
              mismatched_types inferred loc annotated)
        | None ->
          desug_patt env binding.patt inferred (fun env patt ->
            Annot.Ast.binding patt inferred value
              |> kontinue env))

let desug_top env top kontinue = match top with
  | Syntax.Ast.Let top ->
    desug_binding env top.binding (fun env binding ->
      Annot.Ast.top_let binding
        |> kontinue env)
  | Syntax.Ast.Val top ->
    desug_binding env top.binding (fun env binding ->
      Annot.Ast.top_let binding
        |> kontinue env)

let rec desug_tops env tops kontinue = match tops with
  | [] -> kontinue env []
  | top :: tops ->
    desug_top env top (fun env top ->
      desug_tops env tops (fun env tops ->
        top :: tops
          |> kontinue env))

(* TODO: More than just tops in order *)
let desug_file env file kontinue = match file with
  | Syntax.Ast.File file -> desug_tops env file.tops kontinue
