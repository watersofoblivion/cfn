open Printf

open Common
open Parser

(* Initializers *)

let from_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  lexbuf

let from_file path =
  let ic = open_in path in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  Sedlexing.set_filename lexbuf path;
  lexbuf

(* Tokens *)

(* Non-printable *)

let punct_eof = EOF

let new_line lex lexbuf =
  Sedlexing.new_line lexbuf;
  lex lexbuf

(* Punctuation *)

let punct_lparen = LPAREN
let punct_rparen = RPAREN
let punct_pipe = PIPE
let punct_arrow = ARROW
let punct_colon = COLON
let punct_bind = BIND
let punct_ground = GROUND
let punct_squote = SQUOTE
let punct_dquote = DQUOTE

(* Operators *)

let op_un_neg = UN_NEG
let op_un_log_not = UN_LOG_NOT
let op_un_bit_not = UN_BIT_NOT

let op_bin_struct_eq = BIN_STRUCT_EQ
let op_bin_struct_neq = BIN_STRUCT_NEQ
let op_bin_phys_eq = BIN_PHYS_EQ
let op_bin_phys_neq = BIN_PHYS_NEQ
let op_bin_lt = BIN_LT
let op_bin_lte = BIN_LTE
let op_bin_gt = BIN_GT
let op_bin_gte = BIN_GTE
let op_bin_add = BIN_ADD
(* let op_bin_sub = BIN_SUB *) (* Overload of UN_NEG *)
let op_bin_mul = BIN_MUL
let op_bin_div = BIN_DIV
let op_bin_mod = BIN_MOD
let op_bin_exp = BIN_EXP
let op_bin_log_and = BIN_LOG_AND
let op_bin_log_or = BIN_LOG_OR
let op_bin_bit_and = BIN_BIT_AND
(* let op_bin_bit_or = BIN_BIT_OR *) (* Overload of PIPE *)
let op_bin_bit_xor = BIN_BIT_XOR

(* Keywords *)

let kwd_package = PACKAGE
let kwd_from = FROM
let kwd_import = IMPORT
let kwd_let = LET
let kwd_in = IN
let kwd_val = VAL

(* Syntactic Values *)

let lit_bool b = BOOL b

let lit_int lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  INT lexeme

let lit_long lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  LONG lexeme

let lit_float lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  FLOAT lexeme

let lit_double lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  DOUBLE lexeme

let lit_esc_squote = RUNE Utf8.single_quote

let cp_prefix = Str.regexp "\\[Uu]\\+?"
let lit_codepoint lexbuf =
  let cp =
    lexbuf
      |> Sedlexing.Utf8.lexeme
      |> Str.global_replace cp_prefix ""
      |> sprintf "0x%s"
      |> int_of_string
      |> Uchar.of_int
  in
  RUNE cp

let lit_rune lexbuf =
  let lexeme = Sedlexing.lexeme lexbuf in
  RUNE lexeme.(0)

let lit_lident lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  LIDENT lexeme

let lit_uident lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  UIDENT lexeme

(* Regular Expressions *)

let sign = [%sedlex.regexp? '+'|'-']
let decimal = [%sedlex.regexp? '.']
let exponent = [%sedlex.regexp? 'e'|'E']

let binary_radix = [%sedlex.regexp? 'b'|'B']
let octal_radix = [%sedlex.regexp? 'o'|'O']
let hex_radix = [%sedlex.regexp? 'x'|'X']

let binary_digit = [%sedlex.regexp? '0'|'1']
let octal_digit = [%sedlex.regexp? '0'..'7']
let decimal_digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9'|'a'..'f'|'A'..'F']

let int_suffix = [%sedlex.regexp? 'i'|'I']
let long_suffix = [%sedlex.regexp? 'l'|'L']
let float_suffix = [%sedlex.regexp? 'f'|'F']
let double_suffix = [%sedlex.regexp? 'd'|'D']

let whitespace = [%sedlex.regexp? ' '|'\t'|'\r']
let newline = [%sedlex.regexp? '\n']
let ident = [%sedlex.regexp? uppercase|lowercase|'0'..'9'|'_']

let unicode_radix = [%sedlex.regexp? 'u'|'U']

let squote = [%sedlex.regexp? "'"]
let dquote = [%sedlex.regexp? '"']

let esc_squote = [%sedlex.regexp? "\\'"]
let esc_dquote = [%sedlex.regexp? "\\\""]
let esc_lf = [%sedlex.regexp? "\\n"]
let esc_cr = [%sedlex.regexp? "\\r"]
let esc_tab = [%sedlex.regexp? "\\t"]

(* Lexers *)

(*
 * Main lexer
 *)
let rec lex_main lexbuf =
  match%sedlex lexbuf with
    (* Non-printable *)
    | eof             -> punct_eof
    | Plus whitespace -> lex_main lexbuf
    | newline         -> new_line lex_main lexbuf

    (* Punctuation *)
    | '('    -> punct_lparen
    | ')'    -> punct_rparen
    | '|'    -> punct_pipe
    | "->"   -> punct_arrow
    | ':'    -> punct_colon
    | '='    -> punct_bind
    | '_'    -> punct_ground
    | squote -> punct_squote
    | dquote -> punct_dquote

    (* Operators *)
    | '-' -> op_un_neg
    | '!' -> op_un_log_not
    | '~' -> op_un_bit_not

    | "=="  -> op_bin_struct_eq
    | "!="  -> op_bin_struct_neq
    | "!==" -> op_bin_phys_eq
    | "===" -> op_bin_phys_eq
    | "<"   -> op_bin_lt
    | "<="  -> op_bin_lte
    | ">"   -> op_bin_gt
    | ">="  -> op_bin_gte
    | '+'   -> op_bin_add
    | '*'   -> op_bin_mul
    | '/'   -> op_bin_div
    | '%'   -> op_bin_mod
    | "^^"  -> op_bin_exp
    | "&&"  -> op_bin_log_and
    | "||"  -> op_bin_log_or
    | '&'   -> op_bin_bit_and
    | '^'   -> op_bin_bit_xor

    (* Keywords *)
    | "package" -> kwd_package
    | "from"    -> kwd_from
    | "import"  -> kwd_import
    | "let"     -> kwd_let
    | "in"      -> kwd_in
    | "val"     -> kwd_val

    (*
     * Literals
     *)

    (* Booleans *)
    | "true"  -> lit_bool true
    | "false" -> lit_bool false

    (* Integers *)
    | Opt sign, Plus decimal_digit, Opt int_suffix                   -> lit_int lexbuf
    | Opt sign, '0', binary_radix, Plus binary_digit, Opt int_suffix -> lit_int lexbuf
    | Opt sign, '0', octal_radix, Plus octal_digit, Opt int_suffix   -> lit_int lexbuf
    | Opt sign, '0', hex_radix, Plus hex_digit, Opt int_suffix       -> lit_int lexbuf

    (* Longs *)
    | Opt sign, Plus decimal_digit, long_suffix                   -> lit_long lexbuf
    | Opt sign, '0', binary_radix, Plus binary_digit, long_suffix -> lit_long lexbuf
    | Opt sign, '0', octal_radix, Plus octal_digit, long_suffix   -> lit_long lexbuf
    | Opt sign, '0', hex_radix, Plus hex_digit, long_suffix       -> lit_long lexbuf

    (* Floats *)
    | Opt sign, Plus decimal_digit, decimal, Plus decimal_digit, Opt (exponent, Opt sign, Plus decimal_digit), Opt float_suffix -> lit_float lexbuf

    (* Longs *)
    | Opt sign, Plus decimal_digit, decimal, Plus decimal_digit, Opt (exponent, Opt sign, Plus decimal_digit), double_suffix -> lit_double lexbuf

    (* Strings *)
    (* | '"', Star ("\\\"" | Compl '"'), '"' -> lit_string lexbuf *)

    (* Identifiers *)
    | lowercase, Star ident -> lit_lident lexbuf
    | uppercase, Star ident -> lit_uident lexbuf

    (* Error *)
    | _ -> failwith "Lexing error"

(*
 * Lexes the body of a rune.  Allowed syntaxes are:
 *
 * {ul
 *   {li Unicode Escape - [\U+0A].  The 'U' and the hexadecimal digits are case-insensitive and the [+] is optional.}
 *   {li Escape Sequences - Specifically, [\'], [\n], [\r], [\t], [\0]}
 *   {li Literal - Any unicode character that is not a single quote and does not match any of the above. For example, [a] or [ß].}
 * }
 *
 * Also lexes the closing single quote.
 *)
let lex_rune lexbuf =
  match%sedlex lexbuf with
    | esc_squote                                            -> lit_esc_squote
    | esc_lf                                                -> lit_esc_squote
    | esc_cr                                                -> lit_esc_squote
    | esc_tab                                               -> lit_esc_squote
    | '\\', unicode_radix, Opt '+', Rep (hex_digit, 2 .. 6) -> lit_codepoint lexbuf
    (* | Compl ("'" | "\\", ('n' | 't' | 'r'))                 -> lit_rune lexbuf *)
    | squote                                                -> punct_squote
    | _                                                     -> failwith "Lexing error (rune)"

(**
 * Lexes the body of a string.  Allowed syntaxes are:
 *
 * {ul
 *   {li Unicode Escape - [\U+0A].  The 'U' and the hexadecimal digits are case-insensitive and the [+] is optional.}
 *   {li Escape Sequences - Specifically, <double-quote>, [\n], [\r], [\t], [\0]}
 *   {li Literal - A sequence of unicode characters that are not a double quote and do not match any of the above.  For example, [foo bar].}
 * }
 *
 * Also lexes the closing double quote.
 *)
let lex_str lexbuf =
  match%sedlex lexbuf with
    | esc_dquote                                            -> lit_esc_squote
    | esc_lf                                                -> lit_esc_squote
    | esc_cr                                                -> lit_esc_squote
    | esc_tab                                               -> lit_esc_squote
    | '\\', unicode_radix, Opt '+', Rep (hex_digit, 2 .. 6) -> punct_dquote
    (* | Plus (Compl "\\'")                                    -> punct_dquote *)
    | dquote                                                -> punct_dquote
    | _                                                     -> failwith "Lexing error (string segment)"
