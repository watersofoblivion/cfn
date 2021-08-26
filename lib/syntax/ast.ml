open Common

(* Syntax *)

type expr =
  | Bool of { loc: Loc.t; value: bool }
  | Int of { loc: Loc.t; lexeme: string }
  | Long of { loc: Loc.t; lexeme: string }
  | Float of { loc: Loc.t; lexeme: string }
  | Double of { loc: Loc.t; lexeme: string }
  | Rune of { loc: Loc.t; value: Uchar.t }
  | String of { loc: Loc.t; value: Uchar.t list }

type patt =
  | PattGround of { loc: Loc.t }
  | PattVar of { loc: Loc.t; id: Sym.t }

type binding =
  | ValueBinding of { loc: Loc.t; patt: patt; ty: Type.t option; value: expr }

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

type file = File of { pkg: pkg; imports: import list }

(* Constructors *)

let bool loc value = Bool { loc; value }
let int loc lexeme = Int { loc; lexeme }
let long loc lexeme = Long { loc; lexeme }
let float loc lexeme = Float { loc; lexeme }
let double loc lexeme = Double { loc; lexeme }
let rune loc value = Rune { loc; value }
let string loc value = String { loc; value }

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

let file pkg imports = File { pkg; imports }
