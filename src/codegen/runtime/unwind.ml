open Llvm

module type Asm = sig
  module Names : sig
    val raise_exception : string
    val get_language_specific_data : string
    val get_region_start : string
    val get_ip : string
    val set_ip : string
    val set_gr : string
    val resume : string
  end

  val exception_class_t : lltype
  val exception_t : lltype
  val exception_cleanup_fn_t : lltype
  val context_t : lltype

  module ReasonCode : sig
    val t : lltype

    val no_reason : llvalue
    val foreign_exception_caught : llvalue
    val fatal_phase_1_error : llvalue
    val fatal_phase_2_error : llvalue
    val normal_stop : llvalue
    val end_of_stack : llvalue
    val handler_found : llvalue
    val install_context : llvalue
    val continue_unwind : llvalue
  end

  module Action : sig
    val t : lltype

    val search_phase : llvalue
    val cleanup_phase : llvalue
    val handler_frame : llvalue
    val force_unwind : llvalue
    val end_of_stack : llvalue
  end

  val raise_exception: llvalue
  val get_language_specific_data : llvalue
  val get_region_start : llvalue
  val get_ip : llvalue
  val set_ip : llvalue
  val set_gr : llvalue
  val resume : llvalue
end

module Generate (Types: Types.Asm) (Target: Target.Asm) = struct
  module Names = struct
    let raise_exception = "_Unwind_RaiseException"
    let get_language_specific_data = "_Unwind_GetLanguageSpecificData"
    let get_region_start = "_Unwind_GetRegionStart"
    let get_ip = "_Unwind_GetIP"
    let set_ip = "_Unwind_SetIP"
    let set_gr = "_Unwind_SetGR"
    let resume = "_Unwind_Resume"
  end

  let exception_class_t = Types.long_t

  module ReasonCode = struct
    let t = Types.int_t

    let no_reason = const_int t 0
    let foreign_exception_caught = const_int t 1
    let fatal_phase_1_error = const_int t 2
    let fatal_phase_2_error = const_int t 3
    let normal_stop = const_int t 4
    let end_of_stack = const_int t 5
    let handler_found = const_int t 6
    let install_context = const_int t 7
    let continue_unwind = const_int t 8
  end

  let exception_t =
    named_struct_type Target.ctx "unwind-exception-t"

  let exception_cleanup_fn_t =
    function_type Types.void_t [|ReasonCode.t; pointer_type exception_t|]

  let _ =
    struct_set_body exception_t [|ReasonCode.t; exception_cleanup_fn_t; Types.word_t; Types.word_t|] false

  let raise_exception =
    let ty = function_type Types.void_t [|pointer_type exception_t|] in
    declare_function Names.raise_exception ty Target.md

  module Action = struct
    let t = Types.int_t

    let search_phase = const_int t 1
    let cleanup_phase = const_int t 2
    let handler_frame = const_int t 4
    let force_unwind = const_int t 8
    let end_of_stack = const_int t 16
  end

  let context_t = Types.void_ptr_t

  let uint64_t = i64_type Target.ctx

  let get_language_specific_data =
    let ty = function_type uint64_t [|context_t|] in
    declare_function Names.get_language_specific_data ty Target.md

  let get_region_start =
    let ty = function_type uint64_t [|context_t|] in
    declare_function Names.get_region_start ty Target.md

  let get_ip =
    let ty = function_type uint64_t [|context_t|] in
    declare_function Names.get_ip ty Target.md

  let set_ip =
    let ty = function_type Types.void_t [|context_t; uint64_t|] in
    declare_function Names.set_ip ty Target.md

  let set_gr =
    let ty = function_type Types.void_t [|context_t; Types.int_t; uint64_t|] in
    declare_function Names.set_gr ty Target.md

  let resume =
    let ty = function_type Types.void_t [|pointer_type exception_t|] in
    declare_function Names.resume ty Target.md
end
