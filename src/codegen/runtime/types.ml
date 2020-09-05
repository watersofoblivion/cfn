(* Types *)

open Llvm

module type Asm = sig
  module Names : sig
  end

  val bool_t : lltype
  val byte_t : lltype
  val int_t : lltype
  val uint_t : lltype
  val long_t : lltype
  val ulong_t : lltype
  val word_t : lltype
  val void_t : lltype
  val void_ptr_t : lltype
end

module Generate (Target: Target.Asm) = struct
  module Names = struct
  end

  let bool_t = i1_type Target.ctx
  let byte_t = i8_type Target.ctx
  let int_t = i32_type Target.ctx
  let uint_t = int_t
  let long_t = i64_type Target.ctx
  let ulong_t = long_t
  let word_t = i64_type Target.ctx
  let void_t = void_type Target.ctx
  let void_ptr_t = pointer_type (i8_type Target.ctx)
end
