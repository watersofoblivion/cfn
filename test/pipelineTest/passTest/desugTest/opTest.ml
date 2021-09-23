(* Operators *)

open OUnit2

(* Fixtures *)

module TypeSet = Set.Make (struct
  type t = Annot.ty
  let compare = compare
end)

let all_types =
  List.fold_right TypeSet.add [
    Annot.ty_bool;
    Annot.ty_int;
    Annot.ty_long;
    Annot.ty_float;
    Annot.ty_double;
    Annot.ty_rune;
    Annot.ty_string
  ] TypeSet.empty

(* Tests *)

(* Unary *)

(* Binary *)

(* Test Suite *)

let suite =
  "Operators" >::: [
  ]
