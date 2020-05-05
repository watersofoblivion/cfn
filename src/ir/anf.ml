type expr = unit

module ExprTable = Hashtbl.Make(struct
  type t = expr
  let equal _ _ = true
  let hash _ = 0
end)
(*
let exprs = ExprTable.create 251
let hashcons_expr expr =
  try ExprTable.find exprs expr
  with Not_found ->
    ExprTable.add exprs expr expr;
    expr *)
