(* Operators *)

open Format

open Common

(* Syntax *)

type un =
  | UnNeg of { loc: Loc.t }
  | UnLogNot of { loc: Loc.t }
  | UnBitNot of { loc: Loc.t }

type bin =
  | BinStructEq of { loc: Loc.t }
  | BinStructNeq of { loc: Loc.t }
  | BinPhysEq of { loc: Loc.t }
  | BinPhysNeq of { loc: Loc.t }
  | BinLt of { loc: Loc.t }
  | BinLte of { loc: Loc.t }
  | BinGt of { loc: Loc.t }
  | BinGte of { loc: Loc.t }
  | BinAdd of { loc: Loc.t }
  | BinSub of { loc: Loc.t }
  | BinMul of { loc: Loc.t }
  | BinDiv of { loc: Loc.t }
  | BinMod of { loc: Loc.t }
  | BinExp of { loc: Loc.t }
  | BinLogAnd of { loc: Loc.t }
  | BinLogOr of { loc: Loc.t }
  | BinBitAnd of { loc: Loc.t }
  | BinBitOr of { loc: Loc.t }
  | BinBitXor of { loc: Loc.t }

(* Constructors *)

let un_neg loc = UnNeg { loc }
let un_log_not loc = UnLogNot { loc }
let un_bit_not loc = UnBitNot { loc }

let bin_struct_eq loc = BinStructEq { loc }
let bin_struct_neq loc = BinStructNeq { loc }
let bin_phys_eq loc = BinPhysEq { loc }
let bin_phys_neq loc = BinPhysNeq { loc }

let bin_lt loc = BinLt { loc }
let bin_lte loc = BinLte { loc }
let bin_gt loc = BinGt { loc }
let bin_gte loc = BinGte { loc }

let bin_add loc = BinAdd { loc }
let bin_sub loc = BinSub { loc }
let bin_mul loc = BinMul { loc }
let bin_div loc = BinDiv { loc }
let bin_mod loc = BinMod { loc }
let bin_exp loc = BinExp { loc }

let bin_log_and loc = BinLogAnd { loc }
let bin_log_or loc = BinLogOr { loc }

let bin_bit_and loc = BinBitAnd { loc }
let bin_bit_or loc = BinBitOr { loc }
let bin_bit_xor loc = BinBitXor { loc }

(* Operations *)

let loc_un = function
  | UnNeg op -> op.loc
  | UnLogNot op -> op.loc
  | UnBitNot op -> op.loc

let loc_bin = function
  | BinStructEq op -> op.loc
  | BinStructNeq op -> op.loc
  | BinPhysEq op -> op.loc
  | BinPhysNeq op -> op.loc
  | BinLt op -> op.loc
  | BinLte op -> op.loc
  | BinGt op -> op.loc
  | BinGte op -> op.loc
  | BinAdd op -> op.loc
  | BinSub op -> op.loc
  | BinMul op -> op.loc
  | BinDiv op -> op.loc
  | BinMod op -> op.loc
  | BinExp op -> op.loc
  | BinLogAnd op -> op.loc
  | BinLogOr op -> op.loc
  | BinBitAnd op -> op.loc
  | BinBitOr op -> op.loc
  | BinBitXor op -> op.loc

(* Pretty Printing *)

let pp_un fmt = function
  | UnNeg _ -> fprintf fmt "-"
  | UnLogNot _ -> fprintf fmt "!"
  | UnBitNot _ -> fprintf fmt "~"

let pp_bin fmt = function
  | BinStructEq _ -> fprintf fmt "=="
  | BinStructNeq _ -> fprintf fmt "!="
  | BinPhysEq _ -> fprintf fmt "==="
  | BinPhysNeq _ -> fprintf fmt "!=="
  | BinLt _ -> fprintf fmt "<"
  | BinLte _ -> fprintf fmt "<="
  | BinGt _ -> fprintf fmt ">"
  | BinGte _ -> fprintf fmt ">="
  | BinAdd _ -> fprintf fmt "+"
  | BinSub _ -> fprintf fmt "-"
  | BinMul _ -> fprintf fmt "*"
  | BinDiv _ -> fprintf fmt "/"
  | BinMod _ -> fprintf fmt "%%"
  | BinExp _ -> fprintf fmt "^^"
  | BinLogAnd _ -> fprintf fmt "&&"
  | BinLogOr _ -> fprintf fmt "||"
  | BinBitAnd _ -> fprintf fmt "&"
  | BinBitOr _ -> fprintf fmt "|"
  | BinBitXor _ -> fprintf fmt "^"
