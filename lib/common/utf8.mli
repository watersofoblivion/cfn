(** {1 UTF-8 Runes and Strings} *)

(** {2 Runes} *)

val single_quote : Uchar.t
(** [single_quote] is the Unicode character for a single quote [']. *)

val to_string : Uchar.t -> string
(** [to_string uchar] converts the unicode character [uchar] into a UTF-8
    encoded string. *)

(** {2 Strings} *)

val length : string -> int
(** [length str] returns the length of [str] in UTF-8 code points.  Raises
    {!Invalid_argument} if the UTF-8 string is malformed. *)

val normalize : string -> string
(** [normalizes str] NFC normalizes the UTF-8 encoded string [str].  Silently
    replaces malformed encoded Unicode data by a {!Stdlib.Uchar.rep} character. *)
