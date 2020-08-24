open Llvm

module type Asm = sig
  module Names : sig
    val throw : string
    val personality : string
    val begin_catch : string
    val end_catch : string
  end

  val throw : llvalue
  val personality : llvalue
  val begin_catch : llvalue
  val end_catch : llvalue
end

module Generate (Syscall: Syscall.Asm) (Libc: Libc.Asm) (Unwind: Unwind.Asm) (Target: Target.Asm) = struct
  module Names = struct
    let prefix = Target.Names.prefix ^ "::exception-handling::"

    let throw = prefix ^ "throw"
    let personality = prefix ^ "personality"
    let begin_catch = prefix ^ "begin-catch"
    let end_catch = prefix ^ "end-catch"

    let intrinsic_begin_catch = "llvm.eh.begincatch"
    let intrinsic_end_catch = "llvm.eh.endcatch"
  end

  let throw =
    let fn =
      let ty = function_type Libc.void_t [|Libc.void_ptr_t; Libc.void_ptr_t; Libc.void_ptr_t|] in
      define_function Names.throw ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    let one = const_null Libc.int_t in
    ignore (build_call Syscall.exit [|one|] "" builder);
    ignore (build_ret_void builder);

    fn

  let personality =
    let fn =
      let ty = function_type Unwind.ReasonCode.t [|Libc.int_t; Unwind.Action.t; i64_type Target.ctx; pointer_type Unwind.exception_t; Unwind.context_t|] in
      define_function Names.personality ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    ignore (build_ret Unwind.ReasonCode.no_reason builder);

    fn

  let begin_catch =
    let begin_catch_intrinsic =
      let ty = function_type Libc.void_t [|Libc.void_ptr_t; Libc.void_ptr_t|] in
      declare_function Names.intrinsic_begin_catch ty Target.md
    in
    ignore (begin_catch_intrinsic);

    let fn =
      let ty = function_type Libc.void_t [||] in
      define_function Names.begin_catch ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    (* let null_value = const_null (pointer_type (i8_type ctx)) in
    ignore (build_call begin_catch_intrinsic [|null_value; null_value|] "" builder); *)
    ignore (build_ret_void builder);

    fn

  let end_catch =
    let end_catch_intrinsic =
      let ty = function_type Libc.void_t [||] in
      declare_function Names.intrinsic_end_catch ty Target.md
    in
    ignore (end_catch_intrinsic);

    let fn =
      let ty = function_type Libc.void_t [||] in
      define_function Names.end_catch ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    (* ignore (build_call end_catch_intrinsic [||] "" builder); *)
    ignore (build_ret_void builder);

    fn
end
