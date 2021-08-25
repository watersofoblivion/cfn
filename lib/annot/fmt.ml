open Format

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
    fprintf fmt "\"";
