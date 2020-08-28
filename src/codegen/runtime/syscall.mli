open Llvm

module type Asm = sig
  module Names : sig
    val exit : string
  end

  val exit : llvalue
end

module Generate : functor (Types: Types.Asm) -> functor (Target: Target.Asm) -> Asm
