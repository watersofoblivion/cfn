open Parser

open Common

(* Initializers *)

let lexbuf_from_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  let pos = {
    Lexing.pos_fname = "-";
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0;
  } in
  Sedlexing.set_position lexbuf pos;
  lexbuf

let lexbuf_from_file path =
  let ic = open_in path in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  let pos = {
    Lexing.pos_fname = path;
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0;
  } in
  Sedlexing.set_position lexbuf pos;
  Sedlexing.set_filename lexbuf path;
  lexbuf

(* Tokens *)

(* Non-printable *)

let punct_eof = EOF

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
let op_bin_lsl = BIN_LSL
let op_bin_lsr = BIN_LSR
let op_bin_asl = BIN_ASL
let op_bin_asr = BIN_ASR
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

let lit_lident lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  LIDENT lexeme

let lit_uident lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  UIDENT lexeme

(* Regular Expressions *)
let squote = [%sedlex.regexp? "'"]
let dquote = [%sedlex.regexp? '"']

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

let whitespace = [%sedlex.regexp? ' '|'\t'|'\n'|'\r']
let ident = [%sedlex.regexp? uppercase|lowercase|'0'..'9'|'_']

let squote = [%sedlex.regexp? "'"]
let dquote = [%sedlex.regexp? '"']

let esc_bslash = [%sedlex.regexp? "\\\\"]
let esc_lf = [%sedlex.regexp? "\\n"]
let esc_cr = [%sedlex.regexp? "\\r"]
let esc_tab = [%sedlex.regexp? "\\t"]

(*
 * Main lexer
 *)
let rec lex_main lexbuf =
  match%sedlex lexbuf with
    (* Non-printable *)
    | eof             -> punct_eof
    | Plus whitespace -> lex_main lexbuf

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
    | "===" -> op_bin_phys_eq
    | "!==" -> op_bin_phys_neq
    | "<"   -> op_bin_lt
    | "<="  -> op_bin_lte
    | ">"   -> op_bin_gt
    | ">="  -> op_bin_gte
    | "<<"  -> op_bin_lsl
    | ">>"  -> op_bin_lsr
    | "<<<" -> op_bin_asl
    | ">>>" -> op_bin_asr
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

    (* Identifiers *)
    | lowercase, Star ident -> lit_lident lexbuf
    | uppercase, Star ident -> lit_uident lexbuf

    (* Error *)
    | _ -> failwith "Lexing error"

(* Unicode Escapes *)

let lit_codepoint lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  UESC lexeme

let unicode_radix = [%sedlex.regexp? 'u'|'U']
let esc_unicode = [%sedlex.regexp? '\\', unicode_radix, Opt '+', Rep (hex_digit, 4)]

(* Runes *)

let lit_rune_esc chr =
  let cp = Uchar.of_char chr in
  RUNE cp

let lit_rune lexbuf =
  let lexeme = Sedlexing.lexeme lexbuf in
  RUNE lexeme.(0)

let esc_squote = [%sedlex.regexp? "\\'"]

let lex_rune lexbuf =
  match%sedlex lexbuf with
    | eof                 -> punct_eof
    | squote              -> punct_squote
    | esc_bslash          -> lit_rune_esc '\\'
    | esc_squote          -> lit_rune_esc '\''
    | esc_lf              -> lit_rune_esc '\n'
    | esc_cr              -> lit_rune_esc '\r'
    | esc_tab             -> lit_rune_esc '\t'
    | esc_unicode         -> lit_codepoint lexbuf
    | Compl ('\'' | '\\') -> lit_rune lexbuf
    | _                   -> failwith "Lexing error (rune)"

(* String Segments *)

let lit_str_esc chr =
  let lexeme =
    chr
      |> Uchar.of_char
      |> Utf8.to_string
  in
  STRING lexeme

let lit_str_multiline =
  NEWLINE

let lit_str lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  STRING lexeme

let esc_dquote = [%sedlex.regexp? "\\\""]
let multiline = [%sedlex.regexp? "\\\n"]

let lex_str lexbuf =
  match%sedlex lexbuf with
    | eof                       -> punct_eof
    | dquote                    -> punct_dquote
    | esc_bslash                -> lit_str_esc '\\'
    | esc_dquote                -> lit_str_esc '"'
    | esc_lf                    -> lit_str_esc '\n'
    | esc_cr                    -> lit_str_esc '\r'
    | esc_tab                   -> lit_str_esc '\t'
    | multiline                 -> lit_str_multiline
    | esc_unicode               -> lit_codepoint lexbuf
    | Plus (Compl ('"' | '\\')) -> lit_str lexbuf
    | _                         -> failwith "Lexing error (string segment)"

(* Imports *)

let dot = [%sedlex.regexp? '.']
let at = [%sedlex.regexp? '@']
let path_sep = [%sedlex.regexp? '/']
let host = [%sedlex.regexp? Compl (' '|'\t'|'\n'|'\r'|dot|at|path_sep|'?'|'&'|'#'|':')]
let path_seg = [%sedlex.regexp? Compl (' '|'\t'|'\n'|'\r'|dot|at|path_sep|'?'|'&'|'#'|':')]

let punct_whitespace = WHITESPACE

let punct_proto = PROTO
let punct_dot = DOT
let punct_path_sep = PATH_SEP
let punct_at = AT
let punct_v = V

let lit_host lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  HOST lexeme

let lit_path_seg lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  PATH_SEG lexeme

let lit_version lexbuf =
  let lexeme = Sedlexing.Utf8.lexeme lexbuf in
  VERSION lexeme

let lex_import_path lexbuf =
  match%sedlex lexbuf with
    | eof                              -> punct_eof
    | Plus whitespace                  -> punct_whitespace
    | dot                              -> punct_dot
    | "://"                            -> punct_proto
    | at                               -> punct_at
    | path_sep                         -> punct_path_sep
    | "import"                         -> kwd_import
    | Plus (Plus host, dot), Plus host -> lit_host lexbuf
    | Plus path_seg                    -> lit_path_seg lexbuf
    | _                                -> failwith "Lexing error (import path)"

let lex_import_version lexbuf =
  match%sedlex lexbuf with
    | eof                -> punct_eof
    | Plus whitespace    -> punct_whitespace
    | 'v'                -> punct_v
    | "import"           -> kwd_import
    | Plus decimal_digit -> lit_version lexbuf
    | _                  -> failwith "Lexing error (import version)"
