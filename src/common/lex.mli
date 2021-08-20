(**
 * {1 Lexing}
 *)

val from_string : string -> Lexing.lexbuf
(** [from_string s] constructs a lexing buffer from the string [s] with location
    tracking initialized.  The filename is set to ["-"]. *)

val from_channel : ?fname:string -> in_channel -> Lexing.lexbuf
(** [from_channel ?fname ic] constructs a lexing buffer from the input channel
    [ic] with location tracking initialized.  If [fname] is provided, it is used
    as the file name.  Otherwise, ["-"] is used. *)
