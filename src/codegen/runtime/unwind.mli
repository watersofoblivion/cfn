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

module Generate : functor (Types: Types.Asm) -> functor (Target: Target.Asm) -> Asm
