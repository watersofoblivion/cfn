(* Linting *)

type t = unit

(* Constructors *)

(* Operations *)

let pp _ _ = ()

(* Errors *)

type alias = t
module LintSet = Set.Make(struct
  type t = alias
  let compare = compare
end)

type res = LintSet.t

let empty = LintSet.empty
let merge = LintSet.union
