(** {1 UTF-8 Runes and Strings} *)

(** {2 Runes} *)

val single_quote : Uchar.t
(**
  The Unicode character for a single quote ['].

  @since 1.0
*)

val to_string : Uchar.t -> string
(**
  Convert a single unicode character into a UTF-8 encoded string.

  @param uchar The unicode character
  @return The character as a UTF-8 encoded string
  @since 1.0
*)

(** {2 Strings} *)

val length : string -> int
(**
  Get the length of a string in UTF-8 code points.

  @param str The string
  @return The length of the string in UTF-8 code points
  @raise Invalid_argument Raised if the string is malformed UTF-8
  @since 1.0
*)

val normalize : string -> string
(**
  NFC normalize a UTF-8 encoded string.  Silently replaces malformed encoded
  Unicode data by a {!Stdlib.Uchar.rep} character.

  @param str The string to normalize
  @return A NFC-normalized UTF-8 encoded string
  @since 1.0
*)
