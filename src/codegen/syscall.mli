open Llvm

type t

val generate : llmodule -> t

val exit : t -> llvalue
