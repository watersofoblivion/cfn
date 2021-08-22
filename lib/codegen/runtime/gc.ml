open Llvm

module type Asm = sig
  module Names : sig
    val base_ptr : string
    val reset_ptr : string
    val next_ptr : string
    val end_ptr : string

    val from_ptr : string
    val to_ptr : string

    val init : string
    val malloc : string
    val close_perm_gen : string
    val swap_spaces : string
    val init_main_gen : string
    val major : string
  end

  val base_ptr : llvalue
  val reset_ptr : llvalue
  val next_ptr : llvalue
  val end_ptr : llvalue

  val from_ptr : llvalue
  val to_ptr : llvalue

  val init : llvalue
  val malloc : llvalue
  val close_perm_gen : llvalue
  val swap_spaces : llvalue
  val init_main_gen : llvalue
  val major : llvalue
end

module Generate (Types: Types.Asm) (Libc: Libc.Asm) (Target: Target.Asm) = struct
  module Names = struct
    let prefix = Target.Names.prefix ^ "::gc"

    let base_ptr = prefix ^ "::base-ptr"
    let reset_ptr = prefix ^ "::reset-ptr"
    let next_ptr = prefix ^ "::next-ptr"
    let end_ptr = prefix ^ "::end-ptr"
    let from_ptr = prefix ^ "::from-ptr"
    let to_ptr = prefix ^ "::to-ptr"

    let init = prefix ^ "::init"
    let malloc = prefix ^ "::malloc"
    let close_perm_gen = prefix ^ "::close-perm-gen"
    let swap_spaces = prefix ^ "::swap-spaces"
    let init_main_gen = prefix ^ "::init-main-gen"
    let major = prefix ^ "::major"
  end

  let addr_ty = Types.word_t
  let null_addr = const_int addr_ty 0

  let global_void_ptr id = define_global id null_addr Target.md

  let base_ptr = global_void_ptr Names.base_ptr
  let reset_ptr = global_void_ptr Names.reset_ptr
  let next_ptr = global_void_ptr Names.next_ptr
  let end_ptr = global_void_ptr Names.end_ptr
  let from_ptr = global_void_ptr Names.from_ptr
  let to_ptr = global_void_ptr Names.to_ptr

  let init =
    let fn =
      let ty = function_type Types.void_t [|Libc.size_t|] in
      define_function Names.init ty Target.md
    in

    let entry = entry_block fn in
    let builder = builder_at_end Target.ctx entry in

    let size = param fn 0 in
    let malloced_ptr = build_call Libc.malloc [|size|] "malloced_ptr" builder in
    let base_addr = build_ptrtoint malloced_ptr addr_ty "base_addr" builder in
    let _ = build_store base_addr base_ptr builder in
    let _ = build_store base_addr reset_ptr builder in
    let _ = build_store base_addr next_ptr builder in
    let end_addr = build_add base_addr size "end_addr" builder in
    let _ = build_store end_addr end_ptr builder in
    let _ = build_ret_void builder in

    fn

  let malloc =
    let fn =
      let ty = function_type Types.void_ptr_t [|Libc.size_t|] in
      define_function Names.malloc ty Target.md
    in
    let size = param fn 0 in

    let entry = entry_block fn in
    let builder = builder_at_end Target.ctx entry in
    let next_addr = build_load next_ptr "next_addr" builder in
    let end_addr = build_load end_ptr "end_addr" builder in
    let new_next = build_add next_addr size "new_next" builder in

    let oom =
      let bb = append_block Target.ctx "out-of-memory" fn in
      let builder = builder_at_end Target.ctx bb in

      let null_ptr = build_inttoptr null_addr Types.void_ptr_t "null_ptr" builder in
      ignore (build_ret null_ptr builder);

      bb
    in

    let alloc =
      let bb = append_block Target.ctx "allocate" fn in
      let builder = builder_at_end Target.ctx bb in

      ignore (build_store new_next next_ptr builder);
      let next_ptr = build_inttoptr next_addr Types.void_ptr_t "next_ptr" builder in
      ignore (build_ret next_ptr builder);

      bb
    in

    let cond = build_icmp Icmp.Ugt new_next end_addr "cond" builder in
    ignore (build_cond_br cond oom alloc builder);

    fn

  let close_perm_gen =
    let fn =
      let ty = function_type Types.void_t [||] in
      define_function Names.close_perm_gen ty Target.md
    in

    let entry = entry_block fn in
    let builder = builder_at_end Target.ctx entry in

    let base_addr = build_load base_ptr "base_addr" builder in
    let next_addr = build_load next_ptr "next_addr" builder in
    ignore (build_store base_addr from_ptr builder);
    ignore (build_store next_addr to_ptr builder);

    ignore (build_ret_void builder);

    fn

  let swap_spaces =
    let fn =
      let ty = function_type Types.void_t [||] in
      define_function Names.swap_spaces ty Target.md
    in

    let entry = entry_block fn in
    let builder = builder_at_end Target.ctx entry in

    let from_addr = build_load from_ptr "from_addr" builder in
    let to_addr = build_load to_ptr "to_addr" builder in
    ignore (build_store from_addr next_ptr builder);
    ignore (build_store from_addr to_ptr  builder);
    ignore (build_store to_addr from_ptr builder);

    ignore (build_ret_void builder);

    fn

  let init_main_gen =
    let fn =
      let ty = function_type Types.void_t [||] in
      define_function Names.init_main_gen ty Target.md
    in

    let entry = entry_block fn in
    let builder = builder_at_end Target.ctx entry in

    let next_addr = build_load next_ptr "next_addr" builder in
    ignore (build_store next_addr reset_ptr builder);

    ignore (build_ret_void builder);

    fn

  let major =
    let fn =
      let ty = function_type Types.void_t [||] in
      define_function Names.major ty Target.md
    in

    let entry = entry_block fn in
    let builder = builder_at_end Target.ctx entry in
    let reset_addr = build_load reset_ptr "reset_addr" builder in
    ignore (build_store reset_addr next_ptr builder);
    ignore (build_ret_void builder);

    fn
end
