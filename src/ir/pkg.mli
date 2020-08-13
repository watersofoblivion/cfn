(**
 {1 Top-Level Packages}
 *)

type t

val serialize : out_channel -> t -> unit
val deserialize : in_channel -> t
