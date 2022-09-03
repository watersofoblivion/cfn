open Lexing

open Yojson.Safe

(**
 {1 Specification Parsing}
 *)

val json_file : (lexer_state -> lexbuf -> 'a) -> string -> 'a
(**
  Parse a JSON file.  Re-raises any exception raised by the given
  parser.

  @param parser The parser to use
  @param path The path of the JSON file
  @return The aprsed JSON file
  @raise Not_found Raised if the file does not exist
  @since 1.0
*)
