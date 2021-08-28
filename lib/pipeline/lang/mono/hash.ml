open Format

(* Hashing *)

type hash_block = int

let hash_block = 0

let add bytes blk =
  let fn blk ch =
    let blk = blk + (Char.code ch) in
    let blk = blk + (blk lsl 10) in
    blk lxor (blk asr 6)
  in
  let bytes = Bytes.to_seq bytes in
  Seq.fold_left fn blk bytes

let hash_int64 i blk =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_le bytes 0 i;
  add bytes blk

let hash_int32 i blk =
  let bytes = Bytes.create 4 in
  Bytes.set_int32_le bytes 0 i;
  add bytes blk

let hash_int i blk = match Sys.int_size with
  | 31 -> hash_int32 (Int32.of_int i) blk
  | 63 -> hash_int64 (Int64.of_int i) blk
  | int_size ->
    let msg = sprintf "unsupported int size: %d" int_size in
    failwith msg

let hash_char c blk = add (Bytes.make 1 c) blk
let hash_bool b blk = hash_char (if b then Char.chr 1 else Char.chr 0) blk
let hash_float f blk = hash_int64 (Int64.bits_of_float f) blk
let hash_string s blk = add (Bytes.of_string s) blk

let hash_code blk =
  let blk = blk + (blk lsl 3) in
  let blk = blk lxor (blk asr 11) in
  blk + (blk lsl 15)
