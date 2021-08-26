(**
 {1 Lexer}
 *)

open Lexing

(**
 {2 Initializers}
 *)

val from_string : string -> lexbuf
(** [from_string str] initialize a lexing buffer from [str].  Location tracking
    is initialized with the filename [-]. *)

val from_file : string -> lexbuf
(** [from_string path] initialize a lexing buffer from the file located at
    [path].  Location tracking is initialized with the filename [path]. *)

(**
 {2 Entry Point}
 *)

val lex : lexbuf -> Parser.token
(** [lex lexbuf] is the main entry point for the lexer. *)

(**
 {2 Tokens}
 *)

(**
 {3 Non-printable Tokens}
 *)

val punct_eof : Parser.token
(** [punct_eof] constructs an end-of-file token. *)

val new_line : (lexbuf -> Parser.token) -> lexbuf -> Parser.token
(** [new_line lex lexbuf] processes a new line in the input stream and continues
    lexing with [lex]. *)

(**
 {3 Punctuation}
 *)

val punct_pipe : Parser.token
(** [punct_pipe] constructs a token for the [|] symbol. *)

val punct_arrow : Parser.token
(** [punct_arrow] constructs a token for the [->] symbol. *)

val punct_colon : Parser.token
(** [punct_colon] constructs a token for the [:] symbol. *)

val punct_bind : Parser.token
(** [punct_bind] constructs a token for the [=] symbol. *)

val punct_ground : Parser.token
(** [punct_ground] constructs a token for the [_] symbol. *)

(**
 {3 Keywords}
 *)

val kwd_package : Parser.token
(** [kwd_package] constructs a token for the [package] keyword. *)

val kwd_from : Parser.token
(** [kwd_from] constructs a token for the [from] keyword. *)

val kwd_import : Parser.token
(** [kwd_import] constructs a token for the [import] keyword. *)

val kwd_let : Parser.token
(** [kwd_let] constructs a token for the [let] keyword. *)

val kwd_val : Parser.token
(** [kwd_val] constructs a token for the [val] keyword. *)

(**
 {3 Syntactic Values}
 *)

val lit_bool : bool -> Parser.token
(** [lit_bool b] constructs a boolean literal with the value [b]. *)

val lit_int : string -> Parser.token
(** [lit_int lexeme] constructs an integer literal with the lexeme [lexeme]. *)

val lit_long : string -> Parser.token
(** [lit_long lexeme] constructs a long literal with the lexeme [lexeme]. *)

val lit_float : string -> Parser.token
(** [lit_float lexeme] constructs a float literal with the lexeme [lexeme]. *)

val lit_double : string -> Parser.token
(** [lit_double lexeme] constructs a double literal with the lexeme [lexeme]. *)

val lit_rune : Uchar.t -> Parser.token
(** [lit_rune cp] constructs a rune literal of the codepoint [cp]. *)

val lit_string : string -> Parser.token
(** [lit_string cps] constructs a string literal of the string [s]. *)

val lit_lident : string -> Parser.token
(** [lit_lident lexeme] constructs a lower-case identifier with the lexeme
    [lexeme]. *)

val lit_uident : string -> Parser.token
(** [lit_uident lexeme] constructs a upper-case identifier with the lexeme
    [lexeme]. *)
