open Format

(* Symbols *)

type t = {
  sym: int;
  id:  string
}

let sym sym = sym.sym
let id sym =
  if sym.id = ""
  then None
  else Some sym.id

(** Sequences *)

type seq = int ref

let seq _ = ref 0
let gen ?id:(id="") seq =
  let sym = { sym = !seq; id } in
  incr seq;
  sym

(** Equality *)

let equal sym sym' =
  sym.sym = sym'.sym

(** Pretty-Printing *)

let pp sym fmt =
  fprintf fmt "%s$%t" sym.id (Pretty.int sym.sym)

let pp_id sym fmt =
  let invalid_argument sym =
    let msg =
      fprintf str_formatter "Symbol %t does not have an identifier" (pp sym)
        |> flush_str_formatter
    in
    let exn = Invalid_argument msg in
    raise exn
  in
  match id sym with
    | None -> invalid_argument sym
    | Some id -> fprintf fmt "%s" id

(** Tables *)

type alias = t
module Table = Map.Make(struct
  type t = alias
  let compare sym sym' =
    compare sym.sym sym'.sym
end)

type 'a tbl = 'a Table.t

let tbl = Table.empty
let bind sym value tbl kontinue =
  tbl
    |> Table.add sym value
    |> kontinue
let lookup sym tbl = Table.find sym tbl
