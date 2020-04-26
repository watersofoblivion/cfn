open Llvm

type t = {
  base_ptr:  llvalue;
  reset_ptr: llvalue;
  next_ptr:  llvalue;
  end_ptr:   llvalue;

  malloc:              llvalue;
  malloc_with_realloc: llvalue;
  calloc_with_realloc: llvalue;
  realloc:             llvalue;
  strdup:              llvalue;
  free:                llvalue;

  init_perm_gen:  llvalue;
  close_perm_gen: llvalue;
  init_main_gen:  llvalue;
  collect:        llvalue
}

(* Generate *)

let gen ctx md =
  (* Types *)
  let void_ptr_ty = pointer_type (i1_type ctx) in
  let size_ty = i64_type ctx in

  let bool_ty = i64_type ctx in
  let byte_ty = i8_type ctx in

  let gen_ty = i8_type ctx in

  let alloc_ty = named_struct_type ctx "cfn++::gc::alloc_t" in
  let alloc_ptr_ty =
    let ptr_ty = pointer_type alloc_ty in
    struct_set_body alloc_ty [|void_ptr_ty; size_ty; gen_ty; ptr_ty|] false;
    ptr_ty
  in

  let c_str_ty = pointer_type (i8_type ctx) in

  (* Constants *)

  let null_void_ptr = const_int void_ptr_ty 0 in

  let zero = const_int (i64_type ctx) 0 in

  (* let tru = const_int bool_ty 1 in *)
  let fls = const_int bool_ty 0 in

  let null_byte = const_int byte_ty 0 in

  let sys_gen  = const_int gen_ty 0 in
  let perm_gen = const_int gen_ty 1 in
  let main_gen = const_int gen_ty 2 in

  let null_alloc = const_int alloc_ptr_ty 0 in

  (* Global Helpers *)

  let global_void_ptr id = define_global id null_void_ptr md in

  (* Globals *)
  let gen = define_global "cfn++::gc::gen" sys_gen md in

  let base_ptr = global_void_ptr "cfn++::gc::base_ptr" in
  let reset_ptr = global_void_ptr "cfn++::gc::reset_ptr" in
  let next_ptr = global_void_ptr "cfn++::gc::next_ptr" in
  let end_ptr = global_void_ptr "cfn++::gc::end_ptr" in

  let allocs_ptr = define_global "cfn++::gc::allocs" null_alloc md in

  (*
   * // Allocate
   * static void *cfn++::gc::malloc(size_t size) {
   *   void *ptr = next;
   *
   *   if (next + size > end) {
   *     return NULL;
   *   } else {
   *     next += size;
   *     return ptr;
   *   }
   * }
   *)
  let malloc =
    (* Define the function *)
    let fn_ty = function_type void_ptr_ty [|size_ty|] in
    let fn = define_function "cfn++::gc::malloc" fn_ty md in

    let _ =
      (* Parameters *)
      let size = param fn 0 in

      (* Initialize the local pointer *)
      let entry = entry_block fn in
      let ptr =
        let builder = builder_at_end ctx entry in
        build_load next_ptr "ptr" builder
      in

      (* Return null pointer *)
      let oom =
        let bb = append_block ctx "out-of-memory" fn in
        let builder = builder_at_end ctx bb in

        let _ = build_ret null_void_ptr builder in

        bb
      in

      (* Allocate memory *)
      let alloc =
        let bb = append_block ctx "allocate" fn in
        let builder = builder_at_end ctx bb in

        (* Increment next_ptr by size *)
        let local_next_ptr = build_load next_ptr "local_next_ptr" builder in
        let local_next_ptr_int = build_ptrtoint local_next_ptr (i64_type ctx) "local_next_ptr_int" builder in
        let new_next_ptr_int = build_add local_next_ptr_int size "new_next_ptr_int" builder in
        let new_next_ptr = build_inttoptr new_next_ptr_int void_ptr_ty "new_next_ptr" builder in
        let _ = build_store new_next_ptr next_ptr builder in

        (* Return address *)
        let _ = build_ret ptr builder in

        bb
      in

      let builder = builder_at_end ctx entry in

      (* Add size to next pointer *)
      let next_end_int =
        let next_ptr_int = build_ptrtoint next_ptr (i64_type ctx) "next_ptr_int" builder in
        build_add next_ptr_int size "next_end_int" builder
      in

      (* Compare to end pointer *)
      let cond =
        let end_ptr_int = build_ptrtoint end_ptr (i64_type ctx) "end_ptr_int" builder in
        build_icmp Icmp.Ugt next_end_int end_ptr_int "cond" builder
      in

      (* Branch *)
      build_cond_br cond oom alloc builder
    in

    fn
  in

  (*
   * // Allocate with support for re-allocation
   * static void *cfn++::gc::malloc_with_realloc(size_t size) {
   *   void *ptr;
   *   alloc_t *alloc;
   *
   *   ptr = cfn++::gc::malloc(size);
   *
   *   if (ptr) {
   *     alloc = cfn++::gc::malloc(sizeof(alloc_t));
   *     if (alloc) {
   *       alloc->ptr = ptr;
   *       alloc->size = size;
   *       alloc->gen = gen;
   *       alloc->next = allocs;
   *
   *       allocs = alloc;
   *     } else {
   *       return NULL;
   *     }
   *   }
   *
   *   return ptr;
   * }
   *)
  let malloc_with_realloc =
    let fn_ty = function_type void_ptr_ty [|size_ty|] in
    let fn = define_function "cfn++::gc::malloc_with_realloc" fn_ty md in

    let _ =
      (* Parameters *)
      let size = param fn 0 in

      (* Create the pointer and allocation record *)
      let entry = entry_block fn in
      let builder = builder_at_end ctx entry in

      (* Allocate the pointer *)
      let ptr = build_call malloc [|size|] "ptr" builder in

      (* Declare return block *)
      let ret =
        let bb = append_block ctx "return" fn in
        let builder = builder_at_end ctx bb in

        (* Return the allocated pointer *)
        let _ = build_ret ptr builder in

        bb
      in

      (* Successful pointer allocation *)
      let ptr_alloc_success =
        let bb = append_block ctx "ptr_allocation_success" fn in
        let builder = builder_at_end ctx bb in

        (* Allocate the allocation record *)
        let alloc_size = size_of alloc_ty in
        let alloc = build_call malloc [|alloc_size|] "alloc" builder in

        (* Successful allocation record allocation *)
        let alloc_alloc_success =
          let bb = append_block ctx "alloc_allocation_success" fn in
          let builder = builder_at_end ctx bb in

          let value_elem_idx = const_int (i64_type ctx) 1 in

          (* Set the pointer field *)
          let ptr_elem_idx = const_int (i64_type ctx) 0 in
          let ptr_elem_ptr = build_gep alloc [|value_elem_idx; ptr_elem_idx|] "ptr_elem_ptr" builder in
          let _ = build_store ptr ptr_elem_ptr builder in

          (* Set the size field *)
          let size_elem_idx = const_int (i64_type ctx) 1 in
          let size_elem_ptr = build_gep alloc [|value_elem_idx; size_elem_idx|] "size_elem_ptr" builder in
          let _ = build_store size size_elem_ptr builder in

          (* Set the gen field *)
          let gen_elem_idx = const_int (i64_type ctx) 2 in
          let gen_elem_ptr = build_gep alloc [|value_elem_idx; gen_elem_idx|] "gen_elem_ptr" builder in
          let _ = build_store gen gen_elem_ptr builder in

          (* Load the current head of the allocations list *)
          let allocs_hd = build_load allocs_ptr "allocs_hd" builder in

          (* Set the next field *)
          let next_elem_idx = const_int (i64_type ctx) 3 in
          let next_elem_ptr = build_gep alloc [|value_elem_idx; next_elem_idx|] "next_elem_ptr" builder in
          let _ = build_store null_alloc next_elem_ptr builder in

          (* Copy the new head node to the global variable *)
          let _ = build_store allocs_hd allocs_ptr builder in

          (* Branch to the return block *)
          let _ = build_br ret builder in

          bb
        in

        (* Failed allocation record allocation *)
        let alloc_alloc_failure =
          let bb = append_block ctx "alloc_allocation_failure" fn in
          let builder = builder_at_end ctx bb in

          (* Return null pointer *)
          let _ = build_ret null_void_ptr builder in

          bb
        in

        (* Check the result of the allocation *)
        let alloc_int = build_ptrtoint alloc (i64_type ctx) "alloc_int" builder in
        let alloc_cond = build_icmp Icmp.Ne alloc_int zero "alloc_cond" builder in
        let _ = build_cond_br alloc_cond alloc_alloc_success alloc_alloc_failure builder in

        bb
      in

      (* Failed pointer allocation *)
      let ptr_alloc_failure =
        let bb = append_block ctx "ptr_allocation_failure" fn in
        let builder = builder_at_end ctx bb in

        (* Fall through to the return block *)
        let _ = build_br ret builder in

        bb
      in

      let builder = builder_at_end ctx entry in

      (* Check the result of the allocation *)
      let ptr_int = build_ptrtoint ptr (i64_type ctx) "ptr_int" builder in
      let ptr_cond = build_icmp Icmp.Ne ptr_int zero "ptr_cond" builder in
      build_cond_br ptr_cond ptr_alloc_success ptr_alloc_failure builder
    in

    fn
  in

  (*
   * static void *cfn++::gc::calloc_with_realloc(size_t nmemb, size_t size) {
   *   void *ptr;
   *   size_t total_size = nmemb * size;
   *
   *   ptr = cfn++::gc::alloc_with_realloc(total_size);
   *   if (ptr) {
   *     ptr = memset(ptr, '\0', total_size);
   *   }
   *
   *   return ptr;
   * }
   *)
  let calloc_with_realloc =
    let fn_ty = function_type void_ptr_ty [|size_ty; size_ty|] in
    let fn = define_function "cfn++::gc::calloc_with_realloc" fn_ty md in

    let _ =
      (* Memset intrinsic *)
      let memset_ty = function_type (void_type ctx) [|void_ptr_ty; i8_type ctx; size_ty; i1_type ctx|] in
      let memset = declare_function "llvm.memset.p0i8.i64" memset_ty md in

      (* Parameters *)
      let nmemb = param fn 0 in
      let size = param fn 1 in

      let entry = entry_block fn in
      let builder = builder_at_end ctx entry in

      (* Allocate the memory *)
      let total_size = build_mul nmemb size "total_size" builder in
      let ptr = build_call malloc_with_realloc [|total_size|] "ptr" builder in

      (* Return the pointer *)
      let ret =
        let bb = append_block ctx "return" fn in
        let builder = builder_at_end ctx bb in

        let _ = build_ret ptr builder in

        bb
      in

      (* Pointer allocated successfully *)
      let ptr_alloc_success =
        let bb = append_block ctx "ptr_alloc_success" fn in
        let builder = builder_at_end ctx bb in

        (* Clear the memory and branch to the return *)
        let _ = build_call memset [|ptr; null_byte; total_size; fls|] "ignored" builder in
        let _ = build_br ret builder in

        bb
      in

      (* Pointer allocate failed *)
      let ptr_alloc_failure =
        let bb = append_block ctx "ptr_alloc_failure" fn in
        let builder = builder_at_end ctx bb in

        (* Branch to the return *)
        let _ = build_br ret builder in

        bb
      in

      (* Branch based on the allocation result *)
      let ptr_int = build_ptrtoint ptr (i64_type ctx) "ptr_int" builder in
      let ptr_cond = build_icmp Icmp.Ne ptr_int zero "ptr_cond" builder in
      build_cond_br ptr_cond ptr_alloc_success ptr_alloc_failure builder
    in

    fn
  in

  (*
   * static void *cfn++::gc::realloc(void *ptr, size_t newsize) {
   *   alloc_t *alloc = allocs;
   *   void *newptr;
   *
   *   while (alloc) {
   *     if (alloc->ptr == ptr) {
   *       if (alloc->gen != gen || alloc->gen == sys_gen) {
   *         return NULL;
   *       }
   *
   *       if (alloc->size <= newsize) {
   *         return alloc->ptr;
   *       }
   *
   *       newptr = cfn++::gc::malloc_with_realloc(newsize);
   *       if (!newptr) {
   *         return newptr;
   *       }
   *
   *       newptr = memcpy(newptr, alloc->ptr, alloc->size);
   *       alloc->ptr = newptr;
   *       alloc->size = newsize;
   *
   *       return newptr;
   *     } else {
   *       alloc = alloc->next;
   *     }
   *   }
   *
   *   return NULL;
   * }
   *)
  let realloc =
    let fn_ty = function_type void_ptr_ty [|void_ptr_ty; size_ty|] in
    let fn = define_function "cfn++::gc::realloc" fn_ty md in
    fn
  in

  let strdup =
    let fn_ty = function_type c_str_ty [|c_str_ty|] in
    let fn = define_function "cfn++::gc::strdup" fn_ty md in
    fn
  in

  (*
   * static void free(void *ptr) {
   *   // No-op
   * }
   *)
  let free =
    let fn_ty = function_type (void_type ctx) [|void_ptr_ty|] in
    let fn = define_function "cfn++::gc::free" fn_ty md in
    fn
  in

  (*
   * static int cfn++::gc::init_perm_gen(void) {
   *   gen = perm_gen;
   *
   *   base = next;
   *   reset = next;
   * }
   *)
  let init_perm_gen =
    let fn_ty = function_type (i64_type ctx) [||] in
    let fn = define_function "cfn++::gc::init_perm_gen" fn_ty md in

    let _ = perm_gen in

    fn
  in

  (*
   * static int cfn++::gc::close_perm_gen(void) {
   *   reset = next;
   * }
   *)
  let close_perm_gen =
    let fn_ty = function_type (i64_type ctx) [||] in
    let fn = define_function "cfn++::gc::close_perm_gen" fn_ty md in

        let _ = perm_gen in

    fn
  in

  (*
   * static int cfn++::gc::init_main_gen(void) {
   *   // Stop-copy
   *
   *   gen = main_gen;
   *
   *   reset = next - reset;
   *   next = reset;
   * }
   *)
  let init_main_gen =
    let fn_ty = function_type (i64_type ctx) [||] in
    let fn = define_function "cfn++::gc::init_main_gen" fn_ty md in

    let _ = main_gen in

    fn
  in

  (*
   * static int cfn++::gc::collect(void) {
   *   alloc_t *alloc;
   *
   *   while (allocs && allocs->gen == main_gen) {
   *     allocs = allocs->next;
   *   }
   *
   *   alloc = allocs;
   *   while (alloc && alloc->next) {
   *     if (alloc->next->gen == main_gen) {
   *       alloc->next = alloc->next->next;
   *     } else {
   *       alloc = alloc->next;
   *     }
   *   }
   *
   *   next = reset;
   *
   *   return 0;
   * }
   *)
  let collect =
    let fn_ty = function_type (i64_type ctx) [||] in
    let fn = define_function "cfn++::gc::collect" fn_ty md in
    fn
  in

  { base_ptr            = base_ptr;
    reset_ptr           = reset_ptr;
    next_ptr            = next_ptr;
    end_ptr             = end_ptr;

    malloc              = malloc;
    malloc_with_realloc = malloc_with_realloc;
    calloc_with_realloc = calloc_with_realloc;
    realloc             = realloc;
    strdup              = strdup;
    free                = free;

    init_perm_gen       = init_perm_gen;
    close_perm_gen      = close_perm_gen;
    init_main_gen       = init_main_gen;
    collect             = collect }
