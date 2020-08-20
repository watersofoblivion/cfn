open Llvm

let prefix = "cfn++::gc::"

let base_ptr_name = prefix ^ "base-ptr"
let reset_ptr_name = prefix ^ "reset-ptr"
let next_ptr_name = prefix ^ "next-ptr"
let end_ptr_name = prefix ^ "end-ptr"
let from_ptr_name = prefix ^ "from-ptr"
let to_ptr_name = prefix ^ "to-ptr"

let init_name = prefix ^ "init"
let malloc_name = prefix ^ "malloc"
let collect_name = prefix ^ "collect"

(* Generate *)

type t = {
  base_ptr:  llvalue;
  reset_ptr: llvalue;
  next_ptr:  llvalue;
  end_ptr:   llvalue;
  from_ptr:  llvalue;
  to_ptr:    llvalue;

  init:    llvalue;
  malloc:  llvalue;
  collect: llvalue;

  init_perm_gen:  llvalue;
  close_perm_gen: llvalue;
  init_main_gen:  llvalue
}

let base_ptr gc = gc.base_ptr
let reset_ptr gc = gc.reset_ptr
let next_ptr gc = gc.next_ptr
let end_ptr gc = gc.end_ptr
let from_ptr gc = gc.from_ptr
let to_ptr gc = gc.to_ptr

let init gc = gc.init
let malloc gc = gc.malloc
let collect gc = gc.collect

let generate md =
  let ctx = Llvm.module_context md in

  (* Types *)
  let addr_ty = i64_type ctx in
  let size_ty = i64_type ctx in
  let void_ptr_ty = pointer_type (i8_type ctx) in

  (* Constants *)

  let null_addr = const_int addr_ty 0 in

  (* Global Helpers *)

  let global_void_ptr id = define_global id null_addr md in

  (* Globals *)

  let base_ptr = global_void_ptr base_ptr_name in
  let reset_ptr = global_void_ptr reset_ptr_name in
  let next_ptr = global_void_ptr next_ptr_name in
  let end_ptr = global_void_ptr end_ptr_name in
  let from_ptr = global_void_ptr from_ptr_name in
  let to_ptr = global_void_ptr to_ptr_name in

  let malloc =
    let ty = function_type (pointer_type addr_ty) [|size_ty|] in
    declare_function "malloc" ty md
  in

  let init =
    let fn =
      let ty = function_type (void_type ctx) [|size_ty|] in
      define_function init_name ty md
    in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in

    let size = param fn 0 in
    let malloced_ptr = build_call malloc [|size|] "malloced_ptr" builder in
    let base_addr = build_ptrtoint malloced_ptr addr_ty "base_addr" builder in
    let _ = build_store base_addr base_ptr builder in
    let _ = build_store base_addr reset_ptr builder in
    let _ = build_store base_addr next_ptr builder in
    let end_addr = build_add base_addr size "end_addr" builder in
    let _ = build_store end_addr end_ptr builder in
    let _ = build_ret_void builder in

    fn
  in

  let malloc =
    let fn =
      let ty = function_type void_ptr_ty [|size_ty|] in
      define_function malloc_name ty md
    in
    let size = param fn 0 in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in
    let next_addr = build_load next_ptr "next_addr" builder in
    let end_addr = build_load end_ptr "end_addr" builder in
    let new_next = build_add next_addr size "new_next" builder in

    let oom =
      let bb = append_block ctx "out-of-memory" fn in
      let builder = builder_at_end ctx bb in

      let null_ptr = build_inttoptr null_addr void_ptr_ty "null_ptr" builder in
      let _ = build_ret null_ptr builder in

      bb
    in

    let alloc =
      let bb = append_block ctx "allocate" fn in
      let builder = builder_at_end ctx bb in

      let _ = build_store new_next next_ptr builder in
      let next_ptr = build_inttoptr next_addr void_ptr_ty "next_ptr" builder in
      let _ = build_ret next_ptr builder in

      bb
    in

    let cond = build_icmp Icmp.Ugt new_next end_addr "cond" builder in
    let _ = build_cond_br cond oom alloc builder in

    fn
  in

  let collect =
    let fn =
      let ty = function_type (void_type ctx) [||] in
      define_function collect_name ty md
    in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in
    let reset_addr = build_load reset_ptr "reset_addr" builder in
    let _ = build_store reset_addr next_ptr builder in
    let _ = build_ret_void builder in

    fn
  in

  let dummy = const_int addr_ty 42 in
  { base_ptr       = base_ptr;
    reset_ptr      = reset_ptr;
    next_ptr       = next_ptr;
    end_ptr        = end_ptr;
    from_ptr       = from_ptr;
    to_ptr         = to_ptr;

    init           = init;
    malloc         = malloc;
    collect        = collect;

    init_perm_gen  = dummy;
    close_perm_gen = dummy;
    init_main_gen  = dummy }
