open Llvm

module type Asm = sig
  val rol : llvalue -> llvalue -> llbuilder -> llvalue
  val ror : llvalue -> llvalue -> llbuilder -> llvalue

  val process : llbuilder -> llvalue

  module MD5 : sig
    module Names : sig
      val init : string
      val process : string
      val finalize : string
    end

    val f : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val g : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val h : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val i : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue

    val ff : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val gg : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val hh : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val ii : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue

    val state_t : lltype

    val init : llvalue
    val process : llvalue
    val finalize : llvalue
  end

  module SHA256 : sig
    module Names : sig
      val init : string
      val process : string
      val finalize : string
    end

    val ch : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val maj : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue

    val s : llvalue -> llvalue -> llbuilder -> llvalue
    val r : llvalue -> llvalue -> llbuilder -> llvalue

    val sigma_0 : llvalue -> llbuilder -> llvalue
    val sigma_1 : llvalue -> llbuilder -> llvalue

    val gamma_0 : llvalue -> llbuilder -> llvalue
    val gamma_1 : llvalue -> llbuilder -> llvalue

    val state_t : lltype

    val init : llvalue
    val process : llvalue
    val finalize : llvalue
  end

  module HMAC : sig
    module Names : sig
      val init : string
      val process : string
      val finalize : string
    end

    val init : llvalue
    val process : llvalue
    val finalize : llvalue
  end
end

module Generate : functor (Types: Types.Asm) ->
                  functor (Target: Target.Asm) ->
                  Asm
