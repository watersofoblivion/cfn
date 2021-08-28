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
  | Ast.Bool b -> fprintf fmt "%B" b.value
  | Ast.Int i -> fprintf fmt "%ld" i.value
  | Ast.Long l -> fprintf fmt "%Ld" l.value
  | Ast.Float f -> fprintf fmt "%g" f.value
  | Ast.Double d -> fprintf fmt "%g" d.value
  | Ast.Rune r ->
    r.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | Ast.String s ->
    fprintf fmt "\"";
    s.value
      |> List.map Uchar.to_char
      |> List.iter (fprintf fmt "%c");
    fprintf fmt "\""
  | Ast.Ident ident -> Sym.pp fmt ident.id

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
