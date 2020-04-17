{
  open Parser

  (* Initializers *)

  let from_string str =
    let lexbuf = Lexing.from_string str in
    Loc.track "-" lexbuf;
    lexbuf

  let from_file path =
    let ic = open_in path in
    let lexbuf = Lexing.from_channel ic in
    Loc.track path lexbuf;
    lexbuf

  (* Tokens *)

  let deloc = function
    | EOF _ -> EOF Loc.dummy
    | PIPE _ -> PIPE Loc.dummy
    | ARROW _ -> ARROW Loc.dummy
    | PACKAGE _ -> PACKAGE Loc.dummy
    | FROM _ -> FROM Loc.dummy
    | IMPORT _ -> FROM Loc.dummy
    | LIDENT(_, lexeme) -> LIDENT(Loc.dummy, lexeme)
    | STRING(_, lexeme) -> STRING(Loc.dummy, lexeme)

  (* Non-printable *)

  let eof lexbuf =
    let loc = Loc.loc lexbuf in
    Parser.EOF loc

  let new_line lex lexbuf =
    Lexing.new_line lexbuf;
    lex lexbuf

  (* Punctuation *)

  let pipe lexbuf =
    let loc = Loc.loc lexbuf in
    Parser.PIPE loc

  let arrow lexbuf =
    let loc = Loc.loc lexbuf in
    Parser.ARROW loc

  (* Keywords *)

  let package lexbuf =
    let loc = Loc.loc lexbuf in
    Parser.PACKAGE loc

  let from lexbuf =
    let loc = Loc.loc lexbuf in
    Parser.FROM loc

  let import lexbuf =
    let loc = Loc.loc lexbuf in
    Parser.IMPORT loc

  (* Syntactic Values *)

  let lident lexbuf =
    let loc = Loc.loc lexbuf in
    let lexeme = Lexing.lexeme lexbuf in
    Parser.LIDENT (loc, lexeme)

  let quote_re = Str.regexp_string "\\\""
  let string lexbuf =
    let loc = Loc.loc lexbuf in
    let lexeme = Lexing.lexeme lexbuf
              |> Str.global_replace quote_re "\""
    in
    let lexeme = String.sub lexeme 1 (String.length lexeme - 2) in
    Parser.STRING (loc, lexeme)
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

let ident = ['a'-'z' 'A'-'Z' '0'-'9' '_']

(* Entry Point *)
rule lex = parse

(* Non-printable *)
| eof                      { eof lexbuf }
| [' ' '\t' '\r']          { lex lexbuf }
| '\n'                     { new_line lex lexbuf }

(* Punctuation *)
| '|'                      { pipe lexbuf }
| "->"                     { arrow lexbuf }

(* Keywords *)
| "package"                { package lexbuf }
| "from"                   { from lexbuf }
| "import"                 { import lexbuf }

(* Syntactic Values *)
| (lower ident*)           { lident lexbuf }
| '"' ([^'"']|"\\\"")* '"' { string lexbuf }
