(** {1 UTF-8 Runes and Strings} *)

(** {2 Runes} *)

(** {2 Strings} *)

val length : string -> int
(** [length str] returns the length of [str] in UTF-8 code points.  Raises
    {!Invalid_argument} if the UTF-8 string is malformed. *)

val normalize : string -> string
(** [normalizes str] NFC normalizes the UTF-8 encoded string [str].  Silently
    replaces malformed encoded Unicode data by a {!Stdlib.Uchar.rep} character. *)
