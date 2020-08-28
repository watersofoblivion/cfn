open Llvm

module type Asm = sig
  module Names : sig
    val malloc : string
  end

  val size_t : lltype

  val malloc : llvalue
end

module Generate (Types: Types.Asm) (Target: Target.Asm) = struct
  module Names = struct
    let malloc = "malloc"
  end

  let size_t = Types.word_t

  let malloc =
    let ty = function_type Types.void_ptr_t [|size_t|] in
    declare_function Names.malloc ty Target.md
end
