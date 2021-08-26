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
  let punct_colon = COLON
  let punct_bind = BIND
  let punct_ground = GROUND

  (* Keywords *)

  let kwd_package = PACKAGE
  let kwd_from = FROM
  let kwd_import = IMPORT
  let kwd_let = LET
  let kwd_val = VAL

  (* Syntactic Values *)

  let lit_bool b = BOOL b
  let lit_int lexeme = INT lexeme
  let lit_long lexeme = LONG lexeme
  let lit_float lexeme = FLOAT lexeme
  let lit_double lexeme = DOUBLE lexeme
  let lit_rune cp = RUNE cp

  let quote_regex = Str.regexp_string "\\\""
  let lit_string s =
    let cps =
      s
        |> Str.global_replace quote_regex "\""
        |> String.to_seq
        |> List.of_seq
        |> List.map Uchar.of_char
    in
    STRING cps

  let lit_lident lexeme = LIDENT lexeme
  let lit_uident lexeme = UIDENT lexeme
}

let sign = ['+' '-']
let decimal = ['.']
let exponent = ['e' 'E']

let binary_radix = ['b' 'B']
let octal_radix = ['o' 'O']
let hex_radix = ['x' 'X']

let binary_digit = ['0' '1']
let octal_digit = ['0'-'7']
let decimal_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']

let int_suffix = ['i' 'I']
let long_suffix = ['l' 'L']
let float_suffix = ['f' 'F']
let double_suffix = ['d' 'D']

let lower = ['a'-'z']
let upper = ['A'-'Z']
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
| ':'                      { punct_colon }
| '='                      { punct_bind }
| '_'                      { punct_ground }

(* Keywords *)
| "package"                { kwd_package }
| "from"                   { kwd_from }
| "import"                 { kwd_import }
| "let"                    { kwd_let }
| "val"                    { kwd_val }

(*
 * Literals
 *)

(* Booleans *)
| "true"  { lit_bool true }
| "false" { lit_bool false }

(* Integers *)
| sign? decimal_digit+ int_suffix?                 { lexbuf |> Lexing.lexeme |> lit_int }
| sign? '0' binary_radix binary_digit+ int_suffix? { lexbuf |> Lexing.lexeme |> lit_int }
| sign? '0' octal_radix octal_digit+ int_suffix?   { lexbuf |> Lexing.lexeme |> lit_int }
| sign? '0' hex_radix hex_digit+ int_suffix?       { lexbuf |> Lexing.lexeme |> lit_int }

(* Longs *)
| sign? decimal_digit+ long_suffix                 { lexbuf |> Lexing.lexeme |> lit_long }
| sign? '0' binary_radix binary_digit+ long_suffix { lexbuf |> Lexing.lexeme |> lit_long }
| sign? '0' octal_radix octal_digit+ long_suffix   { lexbuf |> Lexing.lexeme |> lit_long }
| sign? '0' hex_radix hex_digit+ long_suffix       { lexbuf |> Lexing.lexeme |> lit_long }

(* Floats *)
| sign? decimal_digit+ decimal decimal_digit+ (exponent sign? decimal_digit+)? float_suffix? { lexbuf |> Lexing.lexeme |> lit_float }

(* Longs *)
| sign? decimal_digit+ decimal decimal_digit+ (exponent sign? decimal_digit+)? double_suffix { lexbuf |> Lexing.lexeme |> lit_double }

(* Runes *)
| "'" ([^'\''] as lexeme) "'" { lit_rune (Uchar.of_char lexeme) }
| "'" ("\\'") "'"             { lit_rune (Uchar.of_char '\'') }

(* Strings *)
| '"' (("\\\"" | [^'"'])* as lexeme) '"' { lit_string lexeme }

(* Identifiers *)
| (lower ident*)           { lexbuf |> Lexing.lexeme |> lit_lident }
| (upper ident*)           { lexbuf |> Lexing.lexeme |> lit_uident }
