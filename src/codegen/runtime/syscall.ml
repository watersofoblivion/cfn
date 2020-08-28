open Llvm

module type Asm = sig
  module Names : sig
    val exit : string
  end

  val exit : llvalue
end

module Generate (Types: Types.Asm) (Target : Target.Asm) = struct
  module Names = struct
    let exit = "exit"
  end

  let exit =
    let ty = function_type Types.void_t [|Types.int_t|] in
    declare_function Names.exit ty Target.md
end
