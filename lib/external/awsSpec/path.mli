open Format

type t = private
  | Blank
  | Ident of string
  | Index of t * int
  | Field of t * string
  | Choice of t * t

val blank : t
val ident : string -> t
val index : t -> int -> t
val field : t -> string -> t
val choice : t -> t -> t

val format : formatter -> t -> unit
val to_string : t -> string
