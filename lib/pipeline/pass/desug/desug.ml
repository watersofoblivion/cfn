open Common

(* Exceptions *)

exception InvalidNumberFormat of Loc.t * string * Sym.t * string
exception UnboundConstructor of Loc.t * Sym.t
exception UnboundIdentifier of Loc.t * Sym.t
exception MismatchedTypes of Annot.ty * Loc.t * Annot.ty
exception UnsupportedBinOpPromotion of Loc.t * Syntax.bin * Loc.t * Annot.ty * Loc.t * Annot.ty

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

(* Types *)

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

(* Runes *)

let desug_rune env r kontinue = match r with
  | _ -> let _ = env in let _ = kontinue in failwith "TODO"

(* Strings *)

let desug_str env s kontinue = match s with
  | _ -> let _ = env in let _ = kontinue in failwith "TODO"

(* Expressions *)

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

let rec desug_expr env expr kontinue = match expr with
  | Syntax.ExprBool expr -> desug_expr_bool env expr.value kontinue
  | Syntax.ExprInt expr -> desug_expr_int env expr.loc expr.lexeme kontinue
  | Syntax.ExprLong expr -> desug_expr_long env expr.loc expr.lexeme kontinue
  | Syntax.ExprFloat expr -> desug_expr_float env expr.loc expr.lexeme kontinue
  | Syntax.ExprDouble expr -> desug_expr_double env expr.loc expr.lexeme kontinue
  | Syntax.ExprRune expr -> desug_expr_rune env expr.value kontinue
  | Syntax.ExprString expr -> desug_expr_string env expr.value kontinue
  | Syntax.ExprIdent expr -> desug_expr_ident env expr.loc expr.id kontinue
  | Syntax.ExprUnOp _ -> failwith "TODO"
  | Syntax.ExprBinOp expr -> desug_expr_bin_op env expr.op expr.lhs expr.rhs kontinue

and desug_expr_bool _ b kontinue =
  b
    |> Annot.expr_bool
    |> kontinue Annot.ty_bool

and desug_expr_int env loc lexeme kontinue =
  lexeme
    |> normalize_int env loc
    |> Annot.expr_int
    |> kontinue Annot.ty_int

and desug_expr_long env loc lexeme kontinue =
  lexeme
    |> normalize_long env loc
    |> Annot.expr_long
    |> kontinue Annot.ty_long

and desug_expr_float env loc lexeme kontinue =
  lexeme
    |> normalize_float env loc
    |> Annot.expr_float
    |> kontinue Annot.ty_float

and desug_expr_double env loc lexeme kontinue =
  lexeme
    |> normalize_double env loc
    |> Annot.expr_double
    |> kontinue Annot.ty_double

and desug_expr_rune env value kontinue =
  desug_rune env value (fun r ->
    r
      |> Annot.expr_rune
      |> kontinue Annot.ty_rune)

and desug_expr_string env value kontinue =
  desug_str env value (fun str ->
    str
      |> Annot.expr_string
      |> kontinue Annot.ty_string)

and desug_expr_ident env loc id kontinue =
  try
    let ty = Env.lookup id env in
    id
      |> Annot.expr_ident
      |> kontinue ty
  with Not_found -> unbound_identifier loc id

and desug_expr_bin_op env op lhs rhs kontinue =
  desug_expr env lhs (fun lty lhs ->
    desug_expr env rhs (fun rty rhs ->
      match op with
        | Syntax.BinAdd _ -> desug_expr_bin_op_numeric env Annot.builtin_add lty rty lhs rhs kontinue
        | Syntax.BinSub _ -> desug_expr_bin_op_numeric env Annot.builtin_sub lty rty lhs rhs kontinue
        | Syntax.BinMul _ -> desug_expr_bin_op_numeric env Annot.builtin_mul lty rty lhs rhs kontinue
        | Syntax.BinDiv _ -> desug_expr_bin_op_numeric env Annot.builtin_div lty rty lhs rhs kontinue
        | Syntax.BinMod _ -> desug_expr_bin_op_integral env Annot.builtin_mod lty rty lhs rhs kontinue
        | Syntax.BinExp _ -> desug_expr_bin_op_floating_point env Annot.builtin_exp lty rty lhs rhs kontinue
      ))

and desug_expr_bin_op_numeric _ builtin lty rty lhs rhs kontinue =
  let promote sub sup expr =
    let builtin = Annot.builtin_promote sub sup in
    Annot.expr_builtin builtin [expr]
  in
  let promote_int_long = promote Annot.ty_int Annot.ty_long in
  let promote_int_double = promote Annot.ty_int Annot.ty_double in
  let promote_float_double = promote Annot.ty_float Annot.ty_double in

  let (ty, lhs_promote, rhs_promote) = match (lty, rty) with
    | Annot.TyInt, Annot.TyInt -> Annot.ty_int, Fun.id, Fun.id
    | Annot.TyLong, Annot.TyLong -> Annot.ty_long, Fun.id, Fun.id
    | Annot.TyFloat, Annot.TyFloat -> Annot.ty_float, Fun.id, Fun.id
    | Annot.TyDouble, Annot.TyDouble -> Annot.ty_double, Fun.id, Fun.id
    | Annot.TyInt, Annot.TyLong -> Annot.ty_long, Fun.id, promote_int_long
    | Annot.TyLong, Annot.TyInt -> Annot.ty_long, promote_int_long, Fun.id
    | Annot.TyInt, Annot.TyFloat -> Annot.ty_double, promote_int_double, promote_float_double
    | Annot.TyFloat, Annot.TyInt -> Annot.ty_double, promote_float_double, promote_int_double
    | Annot.TyInt, Annot.TyDouble -> Annot.ty_double, promote_int_double, Fun.id
    | Annot.TyDouble, Annot.TyInt -> Annot.ty_double, Fun.id, promote_int_double
    | Annot.TyFloat, Annot.TyDouble -> Annot.ty_double, promote_float_double, Fun.id
    | Annot.TyDouble, Annot.TyFloat -> Annot.ty_double, Fun.id, promote_float_double
    | _ -> failwith "TODO"
  in
  let builtin = builtin ty in
  let lhs = lhs_promote lhs in
  let rhs = rhs_promote rhs in
  Annot.expr_builtin builtin [lhs; rhs]
    |> kontinue ty

and desug_expr_bin_op_integral _ builtin lty rty lhs rhs kontinue =
  let promote sub sup expr =
    let builtin = Annot.builtin_promote sub sup in
    Annot.expr_builtin builtin [expr]
  in
  let promote_int_long = promote Annot.ty_int Annot.ty_long in

  let (ty, lhs_promote, rhs_promote) = match (lty, rty) with
    | Annot.TyInt, Annot.TyInt -> Annot.ty_int, Fun.id, Fun.id
    | Annot.TyLong, Annot.TyLong -> Annot.ty_long, Fun.id, Fun.id
    | Annot.TyInt, Annot.TyLong -> Annot.ty_long, Fun.id, promote_int_long
    | Annot.TyLong, Annot.TyInt -> Annot.ty_long, promote_int_long, Fun.id
    | _ -> failwith "TODO"
  in
  let builtin = builtin ty in
  let lhs = lhs_promote lhs in
  let rhs = rhs_promote rhs in
  Annot.expr_builtin builtin [lhs; rhs]
    |> kontinue ty

and desug_expr_bin_op_floating_point _ builtin lty rty lhs rhs kontinue =
  let promote sub sup expr =
    let builtin = Annot.builtin_promote sub sup in
    Annot.expr_builtin builtin [expr]
  in
  let promote_int_double = promote Annot.ty_int Annot.ty_double in
  let promote_float_double = promote Annot.ty_float Annot.ty_double in

  let (ty, lhs_promote, rhs_promote) = match (lty, rty) with
    | Annot.TyInt, Annot.TyInt -> Annot.ty_double, promote_int_double, promote_int_double
    | Annot.TyFloat, Annot.TyFloat -> Annot.ty_float, Fun.id, Fun.id
    | Annot.TyDouble, Annot.TyDouble -> Annot.ty_double, Fun.id, Fun.id
    | Annot.TyInt, Annot.TyFloat -> Annot.ty_double, promote_int_double, promote_float_double
    | Annot.TyFloat, Annot.TyInt -> Annot.ty_double, promote_float_double, promote_int_double
    | Annot.TyInt, Annot.TyDouble -> Annot.ty_double, promote_int_double, Fun.id
    | Annot.TyDouble, Annot.TyInt -> Annot.ty_double, Fun.id, promote_int_double
    | Annot.TyFloat, Annot.TyDouble -> Annot.ty_double, promote_float_double, Fun.id
    | Annot.TyDouble, Annot.TyFloat -> Annot.ty_double, Fun.id, promote_float_double
    | _ -> failwith "TODO"
  in
  let builtin = builtin ty in
  let lhs = lhs_promote lhs in
  let rhs = rhs_promote rhs in
  Annot.expr_builtin builtin [lhs; rhs]
    |> kontinue ty

(* Patterns *)

let rec desug_patt env patt ty kontinue = match patt with
  | Syntax.PattGround _ -> desug_patt_ground env kontinue
  | Syntax.PattVar patt -> desug_patt_var env patt.id ty kontinue

and desug_patt_ground env kontinue =
  Annot.patt_ground
    |> kontinue env

and desug_patt_var env id ty kontinue =
  Env.bind id ty env (fun env ->
    Annot.patt_var id
      |> kontinue env)

(* Bindings *)

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

(* Top-Level Expressions *)

let rec desug_top env top kontinue = match top with
  | Syntax.TopLet top -> desug_top_let env top.binding kontinue
  | Syntax.TopVal top -> desug_top_val env top.binding kontinue

and desug_top_let env binding kontinue =
  desug_binding env binding (fun env binding ->
    Annot.top_let binding
      |> kontinue env)

and desug_top_val env binding kontinue =
  desug_binding env binding (fun env binding ->
    Annot.top_let binding
      |> kontinue env)

(* Files *)

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
