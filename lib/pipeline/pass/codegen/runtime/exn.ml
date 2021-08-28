open Llvm

module type Asm = sig
  module Names : sig
    val throw : string
    val personality : string
    val begin_catch : string
    val end_catch : string
  end

  module Dwarf : sig
    module Names : sig
      val uleb128_decode : string
      val sleb128_decode : string
    end

    val uleb128_decode : llvalue
    val sleb128_decode : llvalue
  end

  val throw : llvalue
  val personality : llvalue
  val begin_catch : llvalue
  val end_catch : llvalue
end

module Generate (Types: Types.Asm) (Libc: Libc.Asm) (Target: Target.Asm) = struct
  module Names = struct
    let prefix = Target.Names.prefix ^ "::exception-handling"

    let throw = prefix ^ "::throw"
    let personality = prefix ^ "::personality"
    let begin_catch = prefix ^ "::begin-catch"
    let end_catch = prefix ^ "::end-catch"

    let intrinsic_begin_catch = "llvm.eh.begincatch"
    let intrinsic_end_catch = "llvm.eh.endcatch"
  end

  module Dwarf = struct
    module Names = struct
      let prefix = Names.prefix ^ "::dwarf"

      let uleb128_decode = prefix ^ "::uleb128-decode"
      let sleb128_decode = prefix ^ "::sleb128-decode"
    end

    let uleb128_decode =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.uleb128_decode ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn

    let sleb128_decode =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.sleb128_decode ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn
  end

  module Unwind = struct
    module Names = struct
      let prefix = Names.prefix ^ "::unwind"

      let exception_t = prefix ^ "::unwind-exception-t"
      (* let raise_exception = "_Unwind_RaiseException" *)
      (* let get_language_specific_data = "_Unwind_LanguageSpecificData" *)
      (* let get_region_start = "_Unwind_GetRegionStart" *)
      (* let get_ip = "_Unwind_GetIP" *)
      (* let set_ip = "_Unwind_SetIP" *)
      (* let set_gr = "_Unwind_SetGR" *)
      (* let resume = "_Unwind_Resume" *)
    end

    (* let exception_class_t = Types.long_t *)

    module ReasonCode = struct
      let t = Types.int_t

      let no_reason = const_int t 0
      (* let foreign_exception_caught = const_int t 1 *)
      (* let fatal_phase_1_error = const_int t 2 *)
      (* let fatal_phase_2_error = const_int t 3 *)
      (* let normal_stop = const_int t 4 *)
      (* let end_of_stack = const_int t 5 *)
      (* let handler_found = const_int t 6 *)
      (* let install_context = const_int t 7 *)
      (* let continue_unwind = const_int t 8 *)
    end

    let exception_t =
      named_struct_type Target.ctx Names.exception_t

    let exception_cleanup_fn_t =
      function_type Types.void_t [|ReasonCode.t; pointer_type exception_t|]

    let _ =
      struct_set_body exception_t [|ReasonCode.t; exception_cleanup_fn_t; Types.word_t; Types.word_t|] false

    (* let raise_exception =
      let ty = function_type Types.void_t [|pointer_type exception_t|] in
      declare_function Names.raise_exception ty Target.md *)

    module Action = struct
      let t = Types.int_t

      (* let search_phase = const_int t 1 *)
      (* let cleanup_phase = const_int t 2 *)
      (* let handler_frame = const_int t 4 *)
      (* let force_unwind = const_int t 8 *)
      (* let end_of_stack = const_int t 16 *)
    end

    let context_t = Types.void_ptr_t

    (* let uint64_t = i64_type Target.ctx *)

    (* let get_language_specific_data =
      let ty = function_type uint64_t [|context_t|] in
      declare_function Names.get_language_specific_data ty Target.md *)

    (* let get_region_start =
      let ty = function_type uint64_t [|context_t|] in
      declare_function Names.get_region_start ty Target.md *)

    (* let get_ip =
      let ty = function_type uint64_t [|context_t|] in
      declare_function Names.get_ip ty Target.md *)

    (* let set_ip =
      let ty = function_type Types.void_t [|context_t; uint64_t|] in
      declare_function Names.set_ip ty Target.md *)

    (* let set_gr =
      let ty = function_type Types.void_t [|context_t; Types.int_t; uint64_t|] in
      declare_function Names.set_gr ty Target.md *)

    (* let resume =
      let ty = function_type Types.void_t [|pointer_type exception_t|] in
      declare_function Names.resume ty Target.md *)
  end

  let throw =
    let fn =
      let ty = function_type Types.void_t [|Types.void_ptr_t; Types.void_ptr_t; Types.void_ptr_t|] in
      define_function Names.throw ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    let one = const_null Types.int_t in
    ignore (build_call Libc.exit [|one|] "" builder);
    ignore (build_ret_void builder);

    fn

  let personality =
    let fn =
      let ty = function_type Unwind.ReasonCode.t [|Types.int_t; Unwind.Action.t; Types.long_t; pointer_type Unwind.exception_t; Unwind.context_t|] in
      define_function Names.personality ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    ignore (build_ret Unwind.ReasonCode.no_reason builder);

    fn

  let begin_catch =
    let begin_catch_intrinsic =
      let ty = function_type Types.void_t [|Types.void_ptr_t; Types.void_ptr_t|] in
      declare_function Names.intrinsic_begin_catch ty Target.md
    in
    ignore (begin_catch_intrinsic);

    let fn =
      let ty = function_type Types.void_t [||] in
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
      let ty = function_type Types.void_t [||] in
      declare_function Names.intrinsic_end_catch ty Target.md
    in
    ignore (end_catch_intrinsic);

    let fn =
      let ty = function_type Types.void_t [||] in
      define_function Names.end_catch ty Target.md
    in

    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in

    (* ignore (build_call end_catch_intrinsic [||] "" builder); *)
    ignore (build_ret_void builder);

    fn
end
