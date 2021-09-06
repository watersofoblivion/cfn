(* Pretty Printing *)

open Format

open Common

let pp_ty fmt = function
  | Type.TyBool -> fprintf fmt "%s" Prim.id_bool
  | Type.TyInt -> fprintf fmt "%s" Prim.id_int
  | Type.TyLong -> fprintf fmt "%s" Prim.id_long
  | Type.TyFloat -> fprintf fmt "%s" Prim.id_float
  | Type.TyDouble -> fprintf fmt "%s" Prim.id_double
  | Type.TyRune -> fprintf fmt "%s" Prim.id_rune
  | Type.TyString -> fprintf fmt "%s" Prim.id_string

let pp_builtin fmt builtin =
  let pp id tys =
    let pp_sep fmt _ = fprintf fmt ", " in
    match tys with
      | [] -> fprintf fmt "%s" id
      | _ -> fprintf fmt "%s[%a]" id (pp_print_list ~pp_sep pp_ty) tys
  in
  match builtin with
    | Builtin.BuiltinAdd builtin -> pp "add" [builtin.ty]
    | Builtin.BuiltinSub builtin -> pp "sub" [builtin.ty]
    | Builtin.BuiltinMul builtin -> pp "mul" [builtin.ty]
    | Builtin.BuiltinDiv builtin -> pp "div" [builtin.ty]
    | Builtin.BuiltinMod builtin -> pp "mod" [builtin.ty]
    | Builtin.BuiltinExp builtin -> pp "exp" [builtin.ty]
    | Builtin.BuiltinPromote builtin -> pp "promote" [builtin.sub; builtin.sup]
    | Builtin.BuiltinConcat builtin -> pp "concat" [builtin.ty]

let pp_patt fmt = function
  | Ast.PattGround -> Pretty.ground fmt
  | Ast.PattVar patt -> Sym.pp fmt patt.id

let rec pp_expr fmt = function
  | Ast.ExprBool expr -> fprintf fmt "%B" expr.value
  | Ast.ExprInt expr -> fprintf fmt "%ld" expr.value
  | Ast.ExprLong expr -> fprintf fmt "%Ld" expr.value
  | Ast.ExprFloat expr -> fprintf fmt "%g" expr.value
  | Ast.ExprDouble expr -> fprintf fmt "%g" expr.value
  | Ast.ExprRune expr -> pp_expr_rune fmt expr.value
  | Ast.ExprString expr -> fprintf fmt "%S" expr.value
  | Ast.ExprIdent expr -> Sym.pp fmt expr.id
  | Ast.ExprBuiltin expr -> pp_expr_builtin fmt expr.fn expr.args

and pp_expr_rune fmt r =
  if r = Utf8.single_quote
  then fprintf fmt "'\\''"
  else
    r
      |> Utf8.to_string
      |> fprintf fmt "'%s'"

and pp_expr_builtin fmt fn args =
  let pp_sep fmt _ = fprintf fmt ",@ " in
  fprintf fmt "%a(%a)" pp_builtin fn (pp_print_list ~pp_sep pp_expr) args

let pp_binding fmt = function
  | Ast.Binding binding ->
    fprintf fmt "%a: %a = %a" pp_patt binding.patt pp_ty binding.ty pp_expr binding.value

let pp_top fmt = function
  | Ast.TopLet top ->
    fprintf fmt "let %a" pp_binding top.binding
