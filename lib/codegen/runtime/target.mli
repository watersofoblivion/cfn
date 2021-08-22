open Llvm

module type Asm = sig
  module Names : sig
    val prefix : string
  end

  val ctx : llcontext
  val md : llmodule
  val finally : unit -> unit
end
