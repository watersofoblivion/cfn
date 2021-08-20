open Lexing

open Yojson.Safe

(**
 {1 Specification Parsing}
 *)

val json_file : (lexer_state -> lexbuf -> 'a) -> string -> 'a option
(** [json_file parser path] parses [path] as a JSON file using [parser].
    Returns [None] if the file does not exists, otherwise returns
    [Some parsed_json].  Re-raises any exception raised by [parser]. *)
