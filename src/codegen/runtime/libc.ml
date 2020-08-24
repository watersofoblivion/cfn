open Llvm

module type Asm = sig
  module Names : sig
    val malloc : string
  end

  val int_t : lltype
  val word_t : lltype
  val size_t : lltype
  val void_t : lltype
  val void_ptr_t : lltype

  val malloc : llvalue
end

module Generate (Target: Target.Asm) = struct
  module Names = struct
    let malloc = "malloc"
  end

  let int_t = i32_type Target.ctx
  let word_t = i64_type Target.ctx
  let size_t = i64_type Target.ctx
  let void_t = void_type Target.ctx
  let void_ptr_t = pointer_type (i8_type Target.ctx)

  let malloc =
    let ty = function_type void_ptr_t [|size_t|] in
    declare_function Names.malloc ty Target.md
end
