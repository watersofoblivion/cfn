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

let pp_atom fmt = function
  | Ast.AtomBool atom -> fprintf fmt "%B" atom.value
  | Ast.AtomInt atom -> fprintf fmt "%ld" atom.value
  | Ast.AtomLong atom -> fprintf fmt "%Ld" atom.value
  | Ast.AtomFloat atom -> fprintf fmt "%g" atom.value
  | Ast.AtomDouble atom -> fprintf fmt "%g" atom.value
  | Ast.AtomRune atom ->
    atom.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | Ast.AtomString atom -> fprintf fmt "%S" atom.value
  | Ast.AtomIdent atom -> Sym.pp fmt atom.id

let rec pp_expr fmt = function
  | Ast.ExprBuiltin expr -> pp_expr_builtin fmt expr.fn expr.args
  | Ast.ExprAtom expr -> pp_atom fmt expr.atom

and pp_expr_builtin fmt fn args =
  let pp_sep fmt _ = fprintf fmt ", " in
  fprintf fmt "%a(%a)" pp_builtin fn (pp_print_list ~pp_sep pp_atom) args

let pp_patt fmt = function
  | Ast.PattGround -> Pretty.ground fmt
  | Ast.PattVar patt -> Sym.pp fmt patt.id

let pp_binding fmt = function
  | Ast.Binding binding ->
    fprintf fmt "%a: %a = %a" pp_patt binding.patt pp_ty binding.ty pp_expr binding.value

let rec pp_block fmt = function
  | Ast.BlockLet block -> pp_block_let fmt block.binding block.scope
  | Ast.BlockExpr block -> pp_expr fmt block.expr

and pp_block_let fmt binding scope =
  fprintf fmt "let %a = %a" pp_binding binding pp_block scope

let pp_top fmt = function
  | Ast.TopLet top ->
    fprintf fmt "let %a" pp_binding top.binding
