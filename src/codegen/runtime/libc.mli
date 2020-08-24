open Llvm

module type Asm = sig
  module Names : sig
    val malloc : string
  end

  val int_t : lltype
  val word_t : lltype
  val size_t : lltype
  val void_t : lltype
  val void_ptr_t : lltype

  val malloc : llvalue
end

module Generate : functor (Target: Target.Asm) -> Asm
