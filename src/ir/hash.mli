(**
 * {1 Hashing}
 *
 * One-at-a-time hashing (taken from
 * {{:https://eternallyconfuzzled.com/hashing-c-introduction-to-hashing}here}.)
 *)

type block
(** A hash block *)

val nil : block
(** [nil] is the empty hash block. *)

val bool : bool -> block -> block
(** [bool b blk] hashes [b] into [blk]. *)

val char : char -> block -> block
(** [char c blk] hashes [c] into [blk]. *)

val int : int -> block -> block
(** [int i blk] hashes [i] into [blk]. *)

val int32 : int32 -> block -> block
(** [int32 i blk] hashes [i] into [blk]. *)

val int64 : int64 -> block -> block
(** [int64 i blk] hashes [i] into [blk]. *)

val float : float -> block -> block
(** [float f blk] hashes [f] into [blk]. *)

val string : string -> block -> block
(** [string s blk] hashes [s] into [blk]. *)

val code : block -> int
(** [code blk] finalizes [blk] and returns the hash code. *)
