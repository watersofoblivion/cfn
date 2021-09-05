(* Operators *)

open Common

type un =
  | OpNeg of { loc: Loc.t }

type bin =
  | OpAdd of { loc: Loc.t }
  | OpSub of { loc: Loc.t }
  | OpMul of { loc: Loc.t }
  | OpDiv of { loc: Loc.t }
  | OpMod of { loc: Loc.t }
  | OpExp of { loc: Loc.t }

(* Constructors *)

let un_neg loc = OpNeg { loc }

let bin_add loc = OpAdd { loc }
let bin_sub loc = OpSub { loc }
let bin_mul loc = OpMul { loc }
let bin_div loc = OpDiv { loc }
let bin_mod loc = OpMod { loc }
let bin_exp loc = OpExp { loc }

(* Operations *)

let loc_un = function
  | OpNeg op -> op.loc

let loc_bin = function
  | OpAdd op -> op.loc
  | OpSub op -> op.loc
  | OpMul op -> op.loc
  | OpDiv op -> op.loc
  | OpMod op -> op.loc
  | OpExp op -> op.loc
