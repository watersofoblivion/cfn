open Llvm

(**
 {1 Types}
 *)

module type Asm = sig
  module Names : sig
  end

  val bool_t : lltype
  val byte_t : lltype
  val int_t : lltype
  val uint_t : lltype
  val long_t : lltype
  val ulong_t : lltype
  val word_t : lltype
  val void_t : lltype
  val void_ptr_t : lltype
end

module Generate : functor (Target: Target.Asm) -> Asm
