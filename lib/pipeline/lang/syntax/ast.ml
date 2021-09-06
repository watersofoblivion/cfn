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
  | ExprString of { loc: Loc.t; value: str list }
  | ExprIdent of { loc: Loc.t; id: Sym.t }
  | ExprUnOp of { loc: Loc.t; op: Op.un; operand: expr }
  | ExprBinOp of { loc: Loc.t; op: Op.bin; lhs: expr; rhs: expr }

type patt =
  | PattGround of { loc: Loc.t }
  | PattVar of { loc: Loc.t; id: Sym.t }

type binding =
  | ValueBinding of { loc: Loc.t; patt: patt; ty: Type.ty option; value: expr }

type top =
  | TopLet of { loc: Loc.t; binding: binding }
  | TopVal of { loc: Loc.t; binding: binding }

type name = Name of { loc: Loc.t; id: Sym.t }

type src = Source of { loc: Loc.t; name: name }
type from = From of { loc: Loc.t; src: src }
type alias = Alias of { loc: Loc.t; pkg: name; alias: name option }
type pkgs = Packages of { loc: Loc.t; pkgs: alias list }
type import = Import of { loc: Loc.t; from: from option; pkgs: pkgs }

type pkg = Package of { loc: Loc.t; id: name }

type file = File of { pkg: pkg; imports: import list; tops: top list }

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

let patt_ground loc = PattGround { loc }
let patt_var loc id = PattVar { loc; id }

let value_binding loc patt ty value = ValueBinding { loc; patt; ty; value }

let top_let loc binding = TopLet { loc; binding }
let top_val loc binding = TopVal { loc; binding }

let name loc id = Name { loc; id }

let src loc name = Source { loc; name }
let from loc src = From { loc; src }
let alias loc pkg alias = Alias { loc; pkg; alias }
let pkgs loc pkgs = Packages { loc; pkgs }
let import loc from pkgs = Import { loc; from; pkgs }

let pkg loc id = Package { loc; id }

let file pkg imports tops = File { pkg; imports; tops }

(* Locations *)

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

let loc_patt = function
  | PattGround patt -> patt.loc
  | PattVar patt -> patt.loc

let loc_binding = function
  | ValueBinding binding -> binding.loc

let loc_top = function
  | TopLet top -> top.loc
  | TopVal top -> top.loc

let loc_name = function
  | Name name -> name.loc

let loc_src = function
  | Source src -> src.loc

let loc_from = function
  | From from -> from.loc

let loc_alias = function
  | Alias alias -> alias.loc

let loc_pkgs = function
  | Packages pkgs -> pkgs.loc

let loc_import = function
  | Import import -> import.loc

let loc_pkg = function
  | Package pkg -> pkg.loc
