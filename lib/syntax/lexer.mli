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

(**
 {3 Keywords}
 *)

val kwd_package : Parser.token
(** [kwd_package] constructs a token for the [package] keyword. *)

val kwd_from : Parser.token
(** [kwd_from] constructs a token for the [from] keyword. *)

val kwd_import : Parser.token
(** [kwd_import] constructs a token for the [import] keyword. *)

(**
 {3 Syntactic Values}
 *)

val lit_lident : string -> Parser.token
(** [lit_lident id] constructs a lower-case identifier token with [id] as the
    value. *)

(* val lit_string : lexbuf -> Parser.token *)
(** [lit_string lexbuf] constructs a string token. *)
