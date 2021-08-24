{
  [@@@coverage exclude_file]
  open Common
  open Parser

  (* Initializers *)

  let from_string str =
    let lexbuf = Lex.from_string str in
    lexbuf

  let from_file path =
    let ic = open_in path in
    let lexbuf = Lex.from_channel ~fname:path ic in
    lexbuf

  (* Tokens *)

  (* Non-printable *)

  let punct_eof = EOF

  let new_line lex lexbuf =
    Lexing.new_line lexbuf;
    lex lexbuf

  (* Punctuation *)

  let punct_pipe = PIPE
  let punct_arrow = ARROW

  (* Keywords *)

  let kwd_package = PACKAGE
  let kwd_from = FROM
  let kwd_import = IMPORT

  (* Syntactic Values *)

  let lit_lident id = LIDENT id

  (* let quote_re = Str.regexp_string "\\\""
  let lit_string lexbuf =
    let lexeme =
      let lexeme =
        Lexing.lexeme lexbuf
          |> Str.global_replace quote_re "\""
      in
      String.sub lexeme 1 (String.length lexeme - 2)
    in
    STRING lexeme *)
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

let ident = ['a'-'z' 'A'-'Z' '0'-'9' '_']

(* Entry Point *)
rule lex = parse

(* Non-printable *)
| eof                      { punct_eof }
| [' ' '\t' '\r']          { lex lexbuf }
| '\n'                     { new_line lex lexbuf }

(* Punctuation *)
| '|'                      { punct_pipe }
| "->"                     { punct_arrow }

(* Keywords *)
| "package"                { kwd_package }
| "from"                   { kwd_from }
| "import"                 { kwd_import }

(* Syntactic Values *)
| (lower ident*)           { lexbuf |> Lexing.lexeme |> lit_lident }
(* | '"' ([^'"']|"\\\"")* '"' { lexbuf |> Lexing.lexeme |> lit_string } *)
