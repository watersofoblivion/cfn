open Common

(* Syntax *)

type name = Name of { loc: Loc.t; id: Sym.t }

type src = Source of { loc: Loc.t; name: name }
type from = From of { loc: Loc.t; src: src }
type alias = Alias of { loc: Loc.t; pkg: name; alias: name option }
type pkgs = Packages of { loc: Loc.t; pkgs: alias list }
type import = Import of { loc: Loc.t; from: from option; pkgs: pkgs }

type pkg = Package of { loc: Loc.t; id: name }

type file = File of { pkg: pkg; imports: import list }

(* Constructors *)

let name loc id = Name { loc; id }

let src loc name = Source { loc; name }
let from loc src = From { loc; src }
let alias loc pkg alias = Alias { loc; pkg; alias }
let pkgs loc pkgs = Packages { loc; pkgs }
let import loc from pkgs = Import { loc; from; pkgs }

let pkg loc id = Package { loc; id }

let file pkg imports = File { pkg; imports }
