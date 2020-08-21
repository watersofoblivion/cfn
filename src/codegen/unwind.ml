open Llvm

type t = {
  word_ty:                 lltype;
  exception_class_ty:      lltype;
  reason_code_ty:          lltype;
  exception_ty:            lltype;
  exception_cleanup_fn_ty: lltype;

  reason_code_no_reason:                llvalue;
  reason_code_foreign_exception_caught: llvalue;
  reason_code_fatal_phase_1_error:      llvalue;
  reason_code_fatal_phase_2_error:      llvalue;
  reason_code_normal_stop:              llvalue;
  reason_code_end_of_stack:             llvalue;
  reason_code_handler_found:            llvalue;
  reason_code_install_context:          llvalue;
  reason_code_continue_unwind:          llvalue;

  raise_exception: llvalue
}

let word_ty unwind = unwind.word_ty
let exception_class_ty unwind = unwind.exception_class_ty
let reason_code_ty unwind = unwind.reason_code_ty
let exception_ty unwind = unwind.exception_ty
let exception_cleanup_fn_ty unwind = unwind.exception_cleanup_fn_ty

let reason_code_no_reason unwind = unwind.reason_code_no_reason
let reason_code_foreign_exception_caught unwind = unwind.reason_code_foreign_exception_caught
let reason_code_fatal_phase_1_error unwind = unwind.reason_code_fatal_phase_1_error
let reason_code_fatal_phase_2_error unwind = unwind.reason_code_fatal_phase_2_error
let reason_code_normal_stop unwind = unwind.reason_code_normal_stop
let reason_code_end_of_stack unwind = unwind.reason_code_end_of_stack
let reason_code_handler_found unwind = unwind.reason_code_handler_found
let reason_code_install_context unwind = unwind.reason_code_install_context
let reason_code_continue_unwind unwind = unwind.reason_code_continue_unwind

let raise_exception unwind = unwind.raise_exception

let generate libc md =
  let ctx = module_context md in

  let void_ty = Libc.void_ty libc in

  let word_ty = i64_type ctx in
  let exception_class_ty = i64_type ctx in

  let reason_code_ty = i32_type ctx in
  let reason_code_no_reason = const_int reason_code_ty 0 in
  let reason_code_foreign_exception_caught = const_int reason_code_ty 1 in
  let reason_code_fatal_phase_1_error = const_int reason_code_ty 2 in
  let reason_code_fatal_phase_2_error = const_int reason_code_ty 3 in
  let reason_code_normal_stop = const_int reason_code_ty 4 in
  let reason_code_end_of_stack = const_int reason_code_ty 5 in
  let reason_code_handler_found = const_int reason_code_ty 6 in
  let reason_code_install_context = const_int reason_code_ty 7 in
  let reason_code_continue_unwind = const_int reason_code_ty 8 in

  let exception_ty =
    named_struct_type ctx "unwind-exception-ty"
  in

  let exception_cleanup_fn_ty =
    function_type void_ty [|reason_code_ty; pointer_type exception_ty|]
  in

  ignore (struct_set_body exception_ty [|
    reason_code_ty;
    exception_cleanup_fn_ty;
    word_ty;
    word_ty;
  |] false);

  let raise_exception =
    let ty = function_type void_ty [|pointer_type exception_ty|] in
    declare_function "_Unwind_RaiseException" ty md
  in

  { word_ty                 = word_ty;
    exception_class_ty      = exception_class_ty;
    reason_code_ty          = reason_code_ty;
    exception_ty            = exception_ty;
    exception_cleanup_fn_ty = exception_cleanup_fn_ty;

    reason_code_no_reason                = reason_code_no_reason;
    reason_code_foreign_exception_caught = reason_code_foreign_exception_caught;
    reason_code_fatal_phase_1_error      = reason_code_fatal_phase_1_error;
    reason_code_fatal_phase_2_error      = reason_code_fatal_phase_2_error;
    reason_code_normal_stop              = reason_code_normal_stop;
    reason_code_end_of_stack             = reason_code_end_of_stack;
    reason_code_handler_found            = reason_code_handler_found;
    reason_code_install_context          = reason_code_install_context;
    reason_code_continue_unwind          = reason_code_continue_unwind;

    raise_exception = raise_exception }
