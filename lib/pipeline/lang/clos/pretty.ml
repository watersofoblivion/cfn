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

let pp_atom fmt = function
  | Ast.Bool atom -> fprintf fmt "%B" atom.value
  | Ast.Int atom -> fprintf fmt "%ld" atom.value
  | Ast.Long atom -> fprintf fmt "%Ld" atom.value
  | Ast.Float atom -> fprintf fmt "%g" atom.value
  | Ast.Double atom -> fprintf fmt "%g" atom.value
  | Ast.Rune atom ->
    atom.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | Ast.String atom -> fprintf fmt "%S" atom.value
  | Ast.Ident atom -> Sym.pp fmt atom.id

let pp_expr fmt = function
  | Ast.Atom expr -> pp_atom fmt expr.atom

let pp_block fmt = function
  | Ast.Expr block -> pp_expr fmt block.expr

let pp_patt fmt = function
  | Ast.PattGround -> Pretty.ground fmt
  | Ast.PattVar patt -> Sym.pp fmt patt.id

let pp_binding fmt = function
  | Ast.Binding binding ->
    fprintf fmt "%a: %a = %a" pp_patt binding.patt pp_ty binding.ty pp_expr binding.value

let pp_top fmt = function
  | Ast.Let top ->
    fprintf fmt "let %a" pp_binding top.binding
