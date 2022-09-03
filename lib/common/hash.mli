(**
 * {1 Hashing}
 *
 * One-at-a-time hashing (taken from
 * {{:https://eternallyconfuzzled.com/hashing-c-introduction-to-hashing}here}.)
 *)

type block
(**
  A hash block.

  @since 1.0
*)

val block : block
(**
  The empty hash block.

  @since 1.0
*)

val bool : bool -> block -> block
(**
  Hash a boolean value into a block.

  @param b The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val char : char -> block -> block
(**
  Hash a character value into a block.

  @param c The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val int : int -> block -> block
(**
  Hash an integer value into a block.

  @param i The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val int32 : int32 -> block -> block
(**
  Hash a 32-bit integer value into a block.

  @param i The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val int64 : int64 -> block -> block
(**
  Hash a 64-bit integer value into a block.

  @param i The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val float : float -> block -> block
(**
  Hash a floating-point value into a block.

  @param f The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val string : string -> block -> block
(**
  Hash a string value into a block.

  @param s The value to hash
  @param blk The block to hash the value into
  @return An updated hash block
  @since 1.0
*)

val code : block -> int
(**
  Finalize a hash block and returns the hash code.

  @param blk The block to finalize
  @return The hash code of the finalized block
  @since 1.0
*)
