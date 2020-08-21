open Llvm

type t

val generate : llmodule -> t

val int_ty : t -> lltype
val word_ty : t -> lltype
val size_ty : t -> lltype
val void_ty : t -> lltype
val void_ptr_ty : t -> lltype

val malloc : t -> llvalue
