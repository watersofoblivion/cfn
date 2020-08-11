(**
 * {1 Lexer}
 *)

val json_path : string -> Path.t
(** [json_path str] parses [s] as a JSON path. *)

val shape_path : string -> Path.t
(** [shape_path str] parses [s] as a shape path. *)
