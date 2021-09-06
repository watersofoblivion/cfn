(* Operators *)

open Common

type un =
  | UnNeg of { loc: Loc.t }

type bin =
  | BinAdd of { loc: Loc.t }
  | BinSub of { loc: Loc.t }
  | BinMul of { loc: Loc.t }
  | BinDiv of { loc: Loc.t }
  | BinMod of { loc: Loc.t }
  | BinExp of { loc: Loc.t }

(* Constructors *)

let un_neg loc = UnNeg { loc }

let bin_add loc = BinAdd { loc }
let bin_sub loc = BinSub { loc }
let bin_mul loc = BinMul { loc }
let bin_div loc = BinDiv { loc }
let bin_mod loc = BinMod { loc }
let bin_exp loc = BinExp { loc }

(* Operations *)

let loc_un = function
  | UnNeg op -> op.loc

let loc_bin = function
  | BinAdd op -> op.loc
  | BinSub op -> op.loc
  | BinMul op -> op.loc
  | BinDiv op -> op.loc
  | BinMod op -> op.loc
  | BinExp op -> op.loc
