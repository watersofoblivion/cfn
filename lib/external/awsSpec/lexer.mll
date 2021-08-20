{
  open Parser
}

(* Source Files *)

let ident  = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let sign   =  ('+'|'-')
let digit  = ['0'-'9']

rule lex_json_path = parse
| eof                      { EOF }
| '['                      { LBRACKET }
| ']'                      { RBRACKET }
| "."                      { DOT }
| "||"                     { OR }
| (sign? digit+) as lexeme { IDX(int_of_string lexeme) }
| ident+ as lexeme         { IDENT lexeme }

and lex_shape_path = parse
| eof              { EOF }
| "$"              { DOT }
| ident+ as lexeme { IDENT lexeme }

{
  let json_path str =
    let lexbuf = Lexing.from_string str in
    Parser.json_path lex_json_path lexbuf

  let shape_path str =
    let lexbuf = Lexing.from_string str in
    Parser.shape_path lex_shape_path lexbuf
}
