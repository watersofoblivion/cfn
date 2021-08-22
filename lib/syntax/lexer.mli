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

val deloc : Parser.token -> Parser.token
(** [deloc tok] replaces the location tracking information in [tok] with
    {!Ast.dummy_loc}. *)

(**
 {3 Non-printable Tokens}
 *)

val eof : lexbuf -> Parser.token
(** [eof lexbuf] constructs an end-of-file token. *)

val new_line : (lexbuf -> Parser.token) -> lexbuf -> Parser.token
(** [new_line lex lexbuf] processes a new line in the input stream and continues
    lexing with [lex]. *)

(**
 {3 Punctuation}
 *)

val pipe : lexbuf -> Parser.token
(** [pipe lexbuf] constructs a token for the [|] symbol. *)

val arrow : lexbuf -> Parser.token
(** [arrow lexbuf] constructs a token for the [->] symbol. *)

(**
 {3 Keywords}
 *)

val package : lexbuf -> Parser.token
(** [package lexbuf] constructs a token for the [package] keyword. *)

val from : lexbuf -> Parser.token
(** [from lexbuf] constructs a token for the [from] keyword. *)

val import : lexbuf -> Parser.token
(** [import lexbuf] constructs a token for the [import] keyword. *)

(**
 {3 Syntactic Values}
 *)

val lident : lexbuf -> Parser.token
(** [lident lexbuf] constructs a lower-case identifier token. *)

val string : lexbuf -> Parser.token
(** [string lexbuf] constructs a string token. *)
