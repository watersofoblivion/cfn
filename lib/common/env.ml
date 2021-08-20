(* Alpha Renaming *)

module Bindings = Map.Make (struct
  type t = string
  let compare = compare
end)

type 'a t = {
  seq: Sym.seq;
  ids: Sym.t Bindings.t;
  tys: Sym.t Bindings.t;
  bound: 'a Sym.tbl
}

(* Constructors *)

let env seq =
  { seq;
    ids   = Bindings.empty;
    tys   = Bindings.empty;
    bound = Sym.tbl }

(* Operations *)

let rename id env kontinue =
  let sym = Sym.gen ~id env.seq in
  let env = { env with ids = Bindings.add id sym env.ids } in
  kontinue env sym

let symbol_of id env =
  try Bindings.find id env.ids
  with Not_found -> Sym.gen ~id env.seq

let constr_of id env kontinue =
  try
    Bindings.find id env.tys
      |> kontinue env
  with Not_found ->
    let sym = Sym.gen ~id env.seq in
    let env = { env with tys = Bindings.add id sym env.tys } in
    kontinue env sym

let bind sym value env kontinue =
  Sym.bind sym value env.bound (fun tbl ->
    { env with bound = tbl }
      |> kontinue)

let lookup sym env = Sym.lookup sym env.bound
