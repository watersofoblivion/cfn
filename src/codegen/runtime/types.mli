open Llvm

(**
 {1 Types}
 *)

module type Asm = sig
  module Names : sig
  end

  val int_t : lltype
  val long_t : lltype
  val word_t : lltype
  val void_t : lltype
  val void_ptr_t : lltype
end

module Generate : functor (Target: Target.Asm) -> Asm
