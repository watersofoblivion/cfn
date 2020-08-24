open Llvm

module type Asm = sig
  module Names : sig
    val prefix : string
  end

  val ctx : llcontext
  val md : llmodule
end
