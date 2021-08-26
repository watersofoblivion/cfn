open Format

open Common

let ty fmt = function
  | Type.Bool -> fprintf fmt "%s" Prim.id_bool
  | Type.Int -> fprintf fmt "%s" Prim.id_int
  | Type.Long -> fprintf fmt "%s" Prim.id_long
  | Type.Float -> fprintf fmt "%s" Prim.id_float
  | Type.Double -> fprintf fmt "%s" Prim.id_double
  | Type.Rune -> fprintf fmt "%s" Prim.id_rune
  | Type.String -> fprintf fmt "%s" Prim.id_string

let expr fmt = function
  | Ast.Bool b -> fprintf fmt "%B" b.value
  | Ast.Int i -> fprintf fmt "%ld" i.value
  | Ast.Long l -> fprintf fmt "%Ld" l.value
  | Ast.Float f -> fprintf fmt "%g" f.value
  | Ast.Double d -> fprintf fmt "%g" d.value
  | Ast.Rune r ->
    r.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | Ast.String s ->
    fprintf fmt "\"";
    s.value
      |> List.map Uchar.to_char
      |> List.iter (fprintf fmt "%c");
    fprintf fmt "\""
  | Ast.Ident ident -> Sym.pp fmt ident.id
