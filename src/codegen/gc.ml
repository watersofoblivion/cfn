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
let close_perm_gen_name = prefix ^ "close-perm-gen"
let swap_spaces_name = prefix ^ "swap-spaces"
let init_main_gen_name = prefix ^ "init-main-gen"
let major_name = prefix ^ "major"

(* Generate *)

type t = {
  base_ptr:  llvalue;
  reset_ptr: llvalue;
  next_ptr:  llvalue;
  end_ptr:   llvalue;
  from_ptr:  llvalue;
  to_ptr:    llvalue;

  init:           llvalue;
  malloc:         llvalue;
  close_perm_gen: llvalue;
  swap_spaces:    llvalue;
  init_main_gen:  llvalue;
  major:          llvalue;
}

let base_ptr gc = gc.base_ptr
let reset_ptr gc = gc.reset_ptr
let next_ptr gc = gc.next_ptr
let end_ptr gc = gc.end_ptr
let from_ptr gc = gc.from_ptr
let to_ptr gc = gc.to_ptr

let init gc = gc.init
let malloc gc = gc.malloc
let close_perm_gen gc = gc.close_perm_gen
let swap_spaces gc = gc.swap_spaces
let init_main_gen gc = gc.init_main_gen
let major gc = gc.major

let generate libc md =
  let ctx = Llvm.module_context md in

  let void_ty = Libc.void_ty libc in
  let void_ptr_ty = Libc.void_ptr_ty libc in
  let size_ty = Libc.size_ty libc in
  let addr_ty = i64_type ctx in
  let null_addr = const_int addr_ty 0 in

  let global_void_ptr id = define_global id null_addr md in

  let base_ptr = global_void_ptr base_ptr_name in
  let reset_ptr = global_void_ptr reset_ptr_name in
  let next_ptr = global_void_ptr next_ptr_name in
  let end_ptr = global_void_ptr end_ptr_name in
  let from_ptr = global_void_ptr from_ptr_name in
  let to_ptr = global_void_ptr to_ptr_name in

  let init =
    let fn =
      let ty = function_type void_ty [|size_ty|] in
      define_function init_name ty md
    in
    let malloc = Libc.malloc libc in

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
      ignore (build_ret null_ptr builder);

      bb
    in

    let alloc =
      let bb = append_block ctx "allocate" fn in
      let builder = builder_at_end ctx bb in

      ignore (build_store new_next next_ptr builder);
      let next_ptr = build_inttoptr next_addr void_ptr_ty "next_ptr" builder in
      ignore (build_ret next_ptr builder);

      bb
    in

    let cond = build_icmp Icmp.Ugt new_next end_addr "cond" builder in
    ignore (build_cond_br cond oom alloc builder);

    fn
  in

  let close_perm_gen =
    let fn =
      let ty = function_type void_ty [||] in
      define_function close_perm_gen_name ty md
    in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in

    let base_addr = build_load base_ptr "base_addr" builder in
    let next_addr = build_load next_ptr "next_addr" builder in
    ignore (build_store base_addr from_ptr builder);
    ignore (build_store next_addr to_ptr builder);

    ignore (build_ret_void builder);

    fn
  in

  let swap_spaces =
    let fn =
      let ty = function_type void_ty [||] in
      define_function swap_spaces_name ty md
    in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in

    let from_addr = build_load from_ptr "from_addr" builder in
    let to_addr = build_load to_ptr "to_addr" builder in
    ignore (build_store from_addr next_ptr builder);
    ignore (build_store from_addr to_ptr  builder);
    ignore (build_store to_addr from_ptr builder);

    ignore (build_ret_void builder);

    fn
  in

  let init_main_gen =
    let fn =
      let ty = function_type void_ty [||] in
      define_function init_main_gen_name ty md
    in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in

    let next_addr = build_load next_ptr "next_addr" builder in
    ignore (build_store next_addr reset_ptr builder);

    ignore (build_ret_void builder);

    fn
  in

  let major =
    let fn =
      let ty = function_type void_ty [||] in
      define_function major_name ty md
    in

    let entry = entry_block fn in
    let builder = builder_at_end ctx entry in
    let reset_addr = build_load reset_ptr "reset_addr" builder in
    ignore (build_store reset_addr next_ptr builder);
    ignore (build_ret_void builder);

    fn
  in

  { base_ptr       = base_ptr;
    reset_ptr      = reset_ptr;
    next_ptr       = next_ptr;
    end_ptr        = end_ptr;
    from_ptr       = from_ptr;
    to_ptr         = to_ptr;

    init           = init;
    malloc         = malloc;
    close_perm_gen = close_perm_gen;
    swap_spaces    = swap_spaces;
    init_main_gen  = init_main_gen;
    major          = major }
