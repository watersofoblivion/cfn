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

module Generate (Types: Types.Asm) (Target: Target.Asm) = struct
  module Names = struct
    let malloc = "malloc"
    let exit = "exit"
  end

  let size_t = Types.word_t

  let malloc =
    let ty = function_type Types.void_ptr_t [|size_t|] in
    declare_function Names.malloc ty Target.md

  let exit =
    let ty = function_type Types.void_t [|Types.int_t|] in
    declare_function Names.exit ty Target.md
end
