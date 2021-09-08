(* Abstract Syntax *)

open Common

(* Exceptions *)

exception InvalidNumberFormat of Loc.t * string * Sym.t * string
exception UnboundIdentifier of Loc.t * Sym.t
exception MismatchedTypes of Annot.ty * Loc.t * Annot.ty
exception UnsupportedBinOpPromotion of Loc.t * Syntax.bin * Loc.t * Annot.ty * Loc.t * Annot.ty

let invalid_number_format loc lexeme constr msg =
  InvalidNumberFormat (loc, lexeme, constr, msg)
    |> raise

let unbound_identifier loc id =
  UnboundIdentifier (loc, id)
    |> raise

let mismatched_types inferred loc annotated =
  MismatchedTypes (inferred, loc, annotated)
    |> raise

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

let promote rule sub value = match rule with
  | None -> value
  | Some sup ->
    let fn = Annot.builtin_promote sub sup in
    Annot.expr_builtin fn [value]

let promote_bin_op op lty lhs rty rhs kontinue =
  let rule = Op.promotion_rule op in
  match rule lty rty with
  | None -> failwith "TODO"
  | Some (lrule, rrule) ->
    let lhs = promote lrule lty lhs in
    let rhs = promote rrule rty rhs in
    kontinue lhs rhs

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
  | Syntax.ExprLet expr -> desug_expr_let env expr.binding expr.scope kontinue

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

and desug_expr_un_op env op operand kontinue =
  desug_expr env operand (fun ty operand ->
    Op.desug_un env op ty (fun res builtin ->
      Annot.expr_builtin builtin [operand]
        |> kontinue res))

and desug_expr_bin_op env op lhs rhs kontinue =
  desug_expr env lhs (fun lty lhs ->
    desug_expr env rhs (fun rty rhs ->
      promote_bin_op op lty lhs rty rhs (fun lhs rhs ->
        Op.desug_bin env op lty rty (fun res builtin ->
          Annot.expr_builtin builtin [lhs; rhs]
            |> kontinue res))))

and desug_expr_let env binding scope kontinue =
  desug_binding env binding (fun env binding ->
    desug_expr env scope (fun ty scope ->
      Annot.expr_let binding scope
        |> kontinue ty))

(* Bindings *)

and desug_binding env binding kontinue = match binding with
  | Syntax.ValueBinding binding ->
    desug_expr env binding.value (fun inferred value ->
      match binding.ty with
        | Some ty ->
          Type.desug_ty env ty (fun annotated ->
            if Annot.ty_equal inferred annotated
            then
              Patt.desug_patt env binding.patt inferred (fun env patt ->
                Annot.binding patt inferred value
                  |> kontinue env)
            else
              let loc = Syntax.loc_ty ty in
              mismatched_types inferred loc annotated)
        | None ->
          Patt.desug_patt env binding.patt inferred (fun env patt ->
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
