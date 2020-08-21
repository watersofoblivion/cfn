open Llvm

type t

val generate : Libc.t -> llmodule -> t

val word_ty : t -> lltype
val exception_class_ty : t -> lltype
val reason_code_ty : t -> lltype
val exception_ty : t -> lltype
val exception_cleanup_fn_ty : t -> lltype

val reason_code_no_reason : t -> llvalue
val reason_code_foreign_exception_caught : t -> llvalue
val reason_code_fatal_phase_1_error : t -> llvalue
val reason_code_fatal_phase_2_error : t -> llvalue
val reason_code_normal_stop : t -> llvalue
val reason_code_end_of_stack : t -> llvalue
val reason_code_handler_found : t -> llvalue
val reason_code_install_context : t -> llvalue
val reason_code_continue_unwind : t -> llvalue

val raise_exception : t -> llvalue
