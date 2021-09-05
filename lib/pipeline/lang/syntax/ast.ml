open Common

(* Syntax *)

type rune =
  | RuneLit of { loc: Loc.t; value: Uchar.t }
  | RuneEscape of { loc: Loc.t; lexeme: string }

type str =
  | StringLit of { loc: Loc.t; lexeme: string }
  | StringEscape of { loc: Loc.t; lexeme: string }

type expr =
  | Bool of { loc: Loc.t; value: bool }
  | Int of { loc: Loc.t; lexeme: string }
  | Long of { loc: Loc.t; lexeme: string }
  | Float of { loc: Loc.t; lexeme: string }
  | Double of { loc: Loc.t; lexeme: string }
  | Rune of { loc: Loc.t; value: rune }
  | String of { loc: Loc.t; value: str list }
  | Ident of { loc: Loc.t; id: Sym.t }
  | UnOp of { loc: Loc.t; op: Op.un; operand: expr }
  | BinOp of { loc: Loc.t; op: Op.bin; lhs: expr; rhs: expr }

type patt =
  | PattGround of { loc: Loc.t }
  | PattVar of { loc: Loc.t; id: Sym.t }

type binding =
  | ValueBinding of { loc: Loc.t; patt: patt; ty: Type.ty option; value: expr }

type top =
  | Let of { loc: Loc.t; binding: binding }
  | Val of { loc: Loc.t; binding: binding }

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

let expr_bool loc value = Bool { loc; value }
let expr_int loc lexeme = Int { loc; lexeme }
let expr_long loc lexeme = Long { loc; lexeme }
let expr_float loc lexeme = Float { loc; lexeme }
let expr_double loc lexeme = Double { loc; lexeme }
let expr_rune loc value = Rune { loc; value }
let expr_string loc value = String { loc; value }
let expr_ident loc id = Ident { loc; id }
let expr_un_op loc op operand = UnOp { loc; op; operand }
let expr_bin_op loc op lhs rhs = BinOp { loc; op; lhs; rhs }

let patt_ground loc = PattGround { loc }
let patt_var loc id = PattVar { loc; id }

let value_binding loc patt ty value = ValueBinding { loc; patt; ty; value }

let top_let loc binding = Let { loc; binding }
let top_val loc binding = Val { loc; binding }

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
  | Bool expr -> expr.loc
  | Int expr -> expr.loc
  | Long expr -> expr.loc
  | Float expr -> expr.loc
  | Double expr -> expr.loc
  | Rune expr -> expr.loc
  | String expr -> expr.loc
  | Ident expr -> expr.loc
  | UnOp expr -> expr.loc
  | BinOp expr -> expr.loc

let loc_patt = function
  | PattGround patt -> patt.loc
  | PattVar patt -> patt.loc

let loc_binding = function
  | ValueBinding binding -> binding.loc

let loc_top = function
  | Let top -> top.loc
  | Val top -> top.loc

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
