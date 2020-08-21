open Llvm

let prefix = "cfn++::exception-handling::"

let throw_name = prefix ^ "throw"
let personality_name = prefix ^ "personality"
let begin_catch_name = prefix ^ "begin-catch"
let end_catch_name = prefix ^ "end-catch"

type t = {
  throw:       llvalue;
  personality: llvalue;
  begin_catch: llvalue;
  end_catch:   llvalue
}

let throw eh = eh.throw
let personality eh = eh.personality
let begin_catch eh = eh.begin_catch
let end_catch eh = eh.end_catch

let generate syscall libc unwind md =
  let ctx = Llvm.module_context md in

  let int_ty = Libc.int_ty libc in
  let void_ty = Libc.void_ty libc in
  let void_ptr_ty = Libc.void_ptr_ty libc in

  let throw =
    let exit = Syscall.exit syscall in
    let unwind = Unwind.raise_exception unwind in
    let fn =
      let ty = function_type void_ty [|void_ptr_ty; void_ptr_ty; void_ptr_ty|] in
      define_function throw_name ty md
    in

    let block = entry_block fn in
    let builder = builder_at_end ctx block in

    ignore (unwind);

    let one = const_null int_ty in
    ignore (build_call exit [|one|] "" builder);
    ignore (build_ret_void builder);

    fn
  in

  let personality =
    let fn =
      let ty = function_type void_ty [||] in
      define_function personality_name ty md
    in

    let block = entry_block fn in
    let builder = builder_at_end ctx block in

    ignore (build_ret_void builder);

    fn
  in

  let begin_catch =
    let begin_catch_intrinsic =
      let ty = function_type void_ty [|void_ptr_ty; void_ptr_ty|] in
      declare_function "llvm.eh.begincatch" ty md
    in
    let fn =
      let ty = function_type void_ty [||] in
      define_function begin_catch_name ty md
    in

    let block = entry_block fn in
    let builder = builder_at_end ctx block in

    ignore (begin_catch_intrinsic);
    (* let null_value = const_null (pointer_type (i8_type ctx)) in
    ignore (build_call begin_catch_intrinsic [|null_value; null_value|] "" builder); *)
    ignore (build_ret_void builder);

    fn
  in

  let end_catch =
    let end_catch_intrinsic =
      let ty = function_type void_ty [||] in
      declare_function "llvm.eh.endcatch" ty md
    in
    let fn =
      let ty = function_type void_ty [||] in
      define_function end_catch_name ty md
    in

    let block = entry_block fn in
    let builder = builder_at_end ctx block in

    ignore (end_catch_intrinsic);
    (* ignore (build_call end_catch_intrinsic [||] "" builder); *)
    ignore (build_ret_void builder);

    fn
  in

  { throw       = throw;
    personality = personality;
    begin_catch = begin_catch;
    end_catch   = end_catch }
