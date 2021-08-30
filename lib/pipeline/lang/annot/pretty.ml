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

let pp_expr fmt = function
  | Ast.Bool expr -> fprintf fmt "%B" expr.value
  | Ast.Int expr -> fprintf fmt "%ld" expr.value
  | Ast.Long expr -> fprintf fmt "%Ld" expr.value
  | Ast.Float expr -> fprintf fmt "%g" expr.value
  | Ast.Double expr -> fprintf fmt "%g" expr.value
  | Ast.Rune expr ->
    expr.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | Ast.String expr -> fprintf fmt "%S" expr.value
  | Ast.Ident expr -> Sym.pp fmt expr.id

let pp_patt fmt = function
  | Ast.PattGround -> Pretty.ground fmt
  | Ast.PattVar patt -> Sym.pp fmt patt.id

let pp_binding fmt = function
  | Ast.Binding binding ->
    fprintf fmt "%a: %a = %a" pp_patt binding.patt pp_ty binding.ty pp_expr binding.value

let pp_top fmt = function
  | Ast.Let top ->
    fprintf fmt "let %a" pp_binding top.binding
