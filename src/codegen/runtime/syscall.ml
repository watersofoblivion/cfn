open Llvm

module type Asm = sig
  module Names : sig
    val exit : string
  end

  val exit : llvalue
end

module Generate (Target : Target.Asm) = struct
  module Names = struct
    let exit = "exit"
  end

  let exit =
    let ty = function_type (void_type Target.ctx) [|i32_type Target.ctx|] in
    declare_function Names.exit ty Target.md
end
