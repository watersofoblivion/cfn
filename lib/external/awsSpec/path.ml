open Format

type t =
  | Blank
  | Ident of string
  | Index of t * int
  | Field of t * string
  | Choice of t * t

let blank = Blank
let ident id = Ident id
let index path idx = Index(path, idx)
let field path field = Field(path, field)
let choice a b = Choice(a, b)

let rec format fmt = function
  | Blank -> ()
  | Ident id -> fprintf fmt "%s" id
  | Index(path, idx) -> fprintf fmt "%a[%d]" format path idx
  | Field(path, field) -> fprintf fmt "%a.%s" format path field
  | Choice(a, b) -> fprintf fmt "%a || %a" format a format b

let to_string path =
  format str_formatter path;
  flush_str_formatter ()
