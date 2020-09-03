open Llvm

module type Asm = sig
  module Names : sig
    val malloc : string
    val exit : string
  end

  val size_t : lltype

  val malloc : llvalue
  val exit : llvalue
end

module Generate : functor (Types: Types.Asm) ->
                  functor (Target: Target.Asm) ->
                  Asm
