open Format

open Common

(* Syntax *)

type rune =
  | RuneLit of { loc: Loc.t; value: Uchar.t }
  | RuneEscape of { loc: Loc.t; lexeme: string }

type str =
  | StringLit of { loc: Loc.t; lexeme: string }
  | StringEscape of { loc: Loc.t; lexeme: string }

type expr =
  | ExprBool of { loc: Loc.t; value: bool }
  | ExprInt of { loc: Loc.t; lexeme: string }
  | ExprLong of { loc: Loc.t; lexeme: string }
  | ExprFloat of { loc: Loc.t; lexeme: string }
  | ExprDouble of { loc: Loc.t; lexeme: string }
  | ExprRune of { loc: Loc.t; value: rune }
  | ExprString of { loc: Loc.t; value: str list list }
  | ExprIdent of { loc: Loc.t; id: Sym.t }
  | ExprUnOp of { loc: Loc.t; op: Op.un; operand: expr }
  | ExprBinOp of { loc: Loc.t; op: Op.bin; lhs: expr; rhs: expr }
  | ExprLet of { loc: Loc.t; binding: binding; scope: expr }

and binding =
  | ValueBinding of { loc: Loc.t; patt: Patt.patt; ty: Type.ty option; value: expr }

type top =
  | TopLet of { loc: Loc.t; binding: binding }
  | TopVal of { loc: Loc.t; binding: binding }

(* Constructors *)

let rune_lit loc value = RuneLit { loc; value }
let rune_escape loc lexeme = RuneEscape { loc; lexeme }

let str_lit loc lexeme = StringLit { loc; lexeme }
let str_escape loc lexeme = StringEscape { loc; lexeme }

let expr_bool loc value = ExprBool { loc; value }
let expr_int loc lexeme = ExprInt { loc; lexeme }
let expr_long loc lexeme = ExprLong { loc; lexeme }
let expr_float loc lexeme = ExprFloat { loc; lexeme }
let expr_double loc lexeme = ExprDouble { loc; lexeme }
let expr_rune loc value = ExprRune { loc; value }
let expr_string loc value = ExprString { loc; value }
let expr_ident loc id = ExprIdent { loc; id }
let expr_un_op loc op operand = ExprUnOp { loc; op; operand }
let expr_bin_op loc op lhs rhs = ExprBinOp { loc; op; lhs; rhs }
let expr_let loc binding scope = ExprLet { loc; binding; scope }

let value_binding loc patt ty value = ValueBinding { loc; patt; ty; value }

let top_let loc binding = TopLet { loc; binding }
let top_val loc binding = TopVal { loc; binding }

(* Locations *)

let loc_rune = function
  | RuneLit rune -> rune.loc
  | RuneEscape rune -> rune.loc

let loc_str = function
  | StringLit str -> str.loc
  | StringEscape str -> str.loc

let loc_expr = function
  | ExprBool expr -> expr.loc
  | ExprInt expr -> expr.loc
  | ExprLong expr -> expr.loc
  | ExprFloat expr -> expr.loc
  | ExprDouble expr -> expr.loc
  | ExprRune expr -> expr.loc
  | ExprString expr -> expr.loc
  | ExprIdent expr -> expr.loc
  | ExprUnOp expr -> expr.loc
  | ExprBinOp expr -> expr.loc
  | ExprLet expr -> expr.loc

let loc_binding = function
  | ValueBinding binding -> binding.loc

let loc_top = function
  | TopLet top -> top.loc
  | TopVal top -> top.loc

(* Pretty Printing *)

(* Runes *)

let rec pp_rune fmt = function
  | RuneLit rune -> pp_rune_lit fmt rune.value
  | RuneEscape rune -> fprintf fmt "%s" rune.lexeme

and pp_rune_lit fmt r =
  if r = Utf8.single_quote
  then fprintf fmt "\\'"
  else
    r
      |> Utf8.to_string
      |> fprintf fmt "%s"

(* Strings *)

let pp_str fmt = function
  | StringLit str -> fprintf fmt "%s" str.lexeme
  | StringEscape str -> fprintf fmt "%s" str.lexeme

(* Expressions *)

let rec pp_expr fmt = function
  | ExprBool expr -> fprintf fmt "%B" expr.value
  | ExprInt expr -> fprintf fmt "%s" expr.lexeme
  | ExprLong expr -> fprintf fmt "%s" expr.lexeme
  | ExprFloat expr -> fprintf fmt "%s" expr.lexeme
  | ExprDouble expr -> fprintf fmt "%s" expr.lexeme
  | ExprRune expr -> fprintf fmt "'%a'" pp_rune expr.value
  | ExprString expr -> pp_expr_string fmt expr.value
  | ExprIdent expr -> Sym.pp_id fmt expr.id
  | ExprUnOp expr -> fprintf fmt "%a%a" Op.pp_un expr.op pp_expr expr.operand
  | ExprBinOp expr -> fprintf fmt "%a %a %a" pp_expr expr.lhs Op.pp_bin expr.op pp_expr expr.rhs
  | ExprLet expr -> pp_expr_let fmt expr.binding expr.scope

and pp_expr_string fmt lines =
  let pp_line fmt segs =
    let pp_sep _ _ = () in
    fprintf fmt "%a" (pp_print_list ~pp_sep pp_str) segs
  in
  let pp_sep fmt _ = fprintf fmt "\\@ " in
  fprintf fmt "\"@[<v>%a@]\"" (pp_print_list ~pp_sep pp_line) lines

and pp_expr_let fmt binding scope =
  fprintf fmt "let %a in %a" pp_binding binding pp_expr scope

(* Bindings *)

and pp_binding fmt = function
  | ValueBinding binding ->
    let pp_ty = pp_print_option (fun fmt t -> fprintf fmt ": %a" Type.pp_ty t) in
    fprintf fmt "%a%a = %a" Patt.pp_patt binding.patt pp_ty binding.ty pp_expr binding.value

(* Top-Level Expressions *)

let pp_top fmt = function
  | TopLet top -> fprintf fmt "let %a" pp_binding top.binding
  | TopVal top -> fprintf fmt "val %a" pp_binding top.binding
