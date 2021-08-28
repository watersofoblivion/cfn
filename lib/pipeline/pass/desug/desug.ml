open Common

exception InvalidNumberFormat of Loc.t * string * Sym.t * string
exception UnboundConstructor of Loc.t * Sym.t
exception UnboundIdentifier of Loc.t * Sym.t
exception MismatchedTypes of Annot.ty * Loc.t * Annot.ty

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
    (Prim.id_bool, Annot.ty_bool);
    (Prim.id_int, Annot.ty_int);
    (Prim.id_long, Annot.ty_long);
    (Prim.id_float, Annot.ty_float);
    (Prim.id_double, Annot.ty_double);
    (Prim.id_rune, Annot.ty_rune);
    (Prim.id_string, Annot.ty_string);
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
      | Syntax.TyConstr constr ->
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
  | Syntax.Bool expr ->
    expr.value
      |> Annot.expr_bool
      |> kontinue Annot.ty_bool
  | Syntax.Int expr ->
    expr.lexeme
      |> normalize_int env expr.loc
      |> Annot.expr_int
      |> kontinue Annot.ty_int
  | Syntax.Long expr ->
    expr.lexeme
      |> normalize_long env expr.loc
      |> Annot.expr_long
      |> kontinue Annot.ty_long
  | Syntax.Float expr ->
    expr.lexeme
      |> normalize_float env expr.loc
      |> Annot.expr_float
      |> kontinue Annot.ty_float
  | Syntax.Double expr ->
    expr.lexeme
      |> normalize_double env expr.loc
      |> Annot.expr_double
      |> kontinue Annot.ty_double
  | Syntax.Rune expr ->
    expr.value
      |> Annot.expr_rune
      |> kontinue Annot.ty_rune
  | Syntax.String expr ->
    expr.value
      |> Annot.expr_string
      |> kontinue Annot.ty_string
  | Syntax.Ident expr ->
    try
      let ty = Env.lookup expr.id env in
      expr.id
        |> Annot.expr_ident
        |> kontinue ty
    with Not_found -> unbound_identifier expr.loc expr.id

let desug_patt env patt ty kontinue = match patt with
  | Syntax.PattGround _ ->
    Annot.patt_ground
      |> kontinue env
  | Syntax.PattVar patt ->
    Env.bind patt.id ty env (fun env ->
      Annot.patt_var patt.id
        |> kontinue env)

let desug_binding env binding kontinue = match binding with
  | Syntax.ValueBinding binding ->
    desug_expr env binding.value (fun inferred value ->
      match binding.ty with
        | Some ty ->
          desug_ty env ty (fun annotated ->
            if Annot.ty_equal inferred annotated
            then
              desug_patt env binding.patt inferred (fun env patt ->
                Annot.binding patt inferred value
                  |> kontinue env)
            else
              let loc = Syntax.loc_ty ty in
              mismatched_types inferred loc annotated)
        | None ->
          desug_patt env binding.patt inferred (fun env patt ->
            Annot.binding patt inferred value
              |> kontinue env))

let desug_top env top kontinue = match top with
  | Syntax.Let top ->
    desug_binding env top.binding (fun env binding ->
      Annot.top_let binding
        |> kontinue env)
  | Syntax.Val top ->
    desug_binding env top.binding (fun env binding ->
      Annot.top_let binding
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
  | Syntax.File file -> desug_tops env file.tops kontinue
