open Format

let ground fmt = fprintf fmt "_"
let bool b fmt = fprintf fmt "%b" b
let int i fmt = fprintf fmt "%d" i

let tops pp_top tops fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp fmt top = pp_top top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp fmt tops;
  fprintf fmt "@]"

let err_at loc fmt k =
  kfprintf k fmt "Error at line %d, col %d: " loc.Loc.start_pos.line loc.start_pos.col
let compiler_bug fmt k =
  kfprintf k fmt "Compiler bug: "
