open Format

(* open Llvm *)
open Ctypes

open Codegen

open OUnit2

let _ =
  if Llvm_executionengine.initialize ()
  then ()
  else failwith "Could not initialize LLVM JIT"

let get_global_var ty name = Llvm_executionengine.get_global_value_address name ty
let get_function ty name = Llvm_executionengine.get_function_address name ty

let get_global_uint64_var = get_global_var uint64_t

let get_base_ptr = get_global_uint64_var Gc.base_ptr_name
let get_reset_ptr = get_global_uint64_var Gc.reset_ptr_name
let get_next_ptr = get_global_uint64_var Gc.next_ptr_name
let get_end_ptr = get_global_uint64_var Gc.end_ptr_name

let get_pointers ee =
  (
    get_base_ptr ee,
    get_reset_ptr ee,
    get_next_ptr ee,
    get_end_ptr ee
  )

let get_init =
  let ty = Foreign.funptr (int64_t @-> returning void) in
  get_function ty Gc.init_name

let get_malloc =
  let ty = Foreign.funptr (int64_t @-> returning (ptr int64_t)) in
  get_function ty Gc.malloc_name

let get_collect =
  let ty = Foreign.funptr (void @-> returning void) in
  get_function ty Gc.collect_name

let llvm_test setup tester ctxt =
  let ctx = Llvm.create_context () in
  let finally _ = Llvm.dispose_context ctx in

  let md = Llvm.create_module ctx "test-module" in
  let _ = setup md in
  let _ = match Llvm_analysis.verify_module md with
    | None -> ()
    | Some msg ->
      Llvm.dump_module md;
      let msg = sprintf "Invalid module: %s\n%!" msg in
      assert_failure msg
  in
  let ee = Llvm_executionengine.create md in
  Fun.protect ~finally (fun _ -> tester ee ctxt)

let test_generate =
  let printer x = sprintf "%x" (Unsigned.UInt64.to_int x) in

  let test_initialize =
    let test_valid ee ctxt =
      let zero = Unsigned.UInt64.of_int64 (Int64.zero) in

      let heap_size = Int64.of_int 1024 in
      let init = get_init ee in

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = get_pointers ee in
      assert_equal ~ctxt ~msg:"GC Base Pointer" zero base_ptr;
      assert_equal ~ctxt ~msg:"GC Reset Pointer" zero reset_ptr;
      assert_equal ~ctxt ~msg:"GC Next Pointer" zero next_ptr;
      assert_equal ~ctxt ~msg:"GC End Pointer" zero end_ptr;

      init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = get_pointers ee in
      let expected_end =
        let heap_size = Unsigned.UInt64.of_int64 heap_size in
        Unsigned.UInt64.add base_ptr heap_size
      in
      let cmp x y = not (x = y) in
      assert_equal ~ctxt ~cmp ~msg:"GC Base Pointer" zero base_ptr;
      assert_equal ~ctxt ~msg:"GC Reset Pointer" base_ptr reset_ptr;
      assert_equal ~ctxt ~msg:"GC Next Pointer"base_ptr next_ptr;
      assert_equal ~ctxt ~msg:"GC End Pointer" expected_end end_ptr;
    in
    "Initialize" >::: [
      "Valid" >:: llvm_test Gc.generate test_valid
    ]
  in
  let test_malloc =
    let test_valid ee ctxt =
      let heap_size = Int64.of_int 1024 in
      let init = get_init ee in

      let malloc = get_malloc ee in

      init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = get_pointers ee in
      let size_one = Int64.of_int 8 in
      let expected_next =
        let size = Unsigned.UInt64.of_int64 size_one in
        Unsigned.UInt64.add next_ptr size
      in
      let addr_one =
        size_one
          |> malloc
          |> to_voidp
          |> raw_address_of_ptr
          |> Int64.of_nativeint
          |> Unsigned.UInt64.of_int64
      in

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = get_pointers ee in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_next new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
      assert_equal ~ctxt ~printer ~msg:"Returned Address" next_ptr addr_one;

      let size_two = Int64.of_int 16 in
      let expected_addr = new_next_ptr in
      let expected_next =
        let size = Unsigned.UInt64.of_int64 size_two in
        Unsigned.UInt64.add expected_next size
      in
      let addr_two =
        size_two
          |> malloc
          |> to_voidp
          |> raw_address_of_ptr
          |> Int64.of_nativeint
          |> Unsigned.UInt64.of_int64
      in

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = get_pointers ee in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_next new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
      assert_equal ~ctxt ~printer ~msg:"Returned Address" expected_addr addr_two
    in
    let test_invalid =
      let test_out_of_memory ee ctxt =
        let zero = Unsigned.UInt64.of_int64 (Int64.zero) in

        let heap_size = Int64.of_int 4 in
        let init = get_init ee in

        let malloc = get_malloc ee in

        init heap_size;

        let (base_ptr, reset_ptr, next_ptr, end_ptr) = get_pointers ee in
        let addr =
          let alloc_size = Int64.of_int 8 in
          alloc_size
            |> malloc
            |> to_voidp
            |> raw_address_of_ptr
            |> Int64.of_nativeint
            |> Unsigned.UInt64.of_int64
        in

        let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = get_pointers ee in
        assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" next_ptr new_next_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
        assert_equal ~ctxt ~printer ~msg:"Returned Address" zero addr;
      in
      "Invalid" >::: [
        "Out of Memory" >:: llvm_test Gc.generate test_out_of_memory
      ]
    in
    "Malloc" >::: [
      "Valid" >:: llvm_test Gc.generate test_valid;
      test_invalid
    ]
  in
  let test_collect =
    let test_valid ee ctxt =
      let heap_size = Int64.of_int 1024 in
      let init = get_init ee in
      let malloc = get_malloc ee in
      let collect = get_collect ee in

      init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = get_pointers ee in
      let size = Int64.of_int 8 in
      let expected_next =
        let size = Unsigned.UInt64.of_int64 size in
        Unsigned.UInt64.add next_ptr size
      in
      let _ = malloc size in

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = get_pointers ee in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_next new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;

      collect ();

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = get_pointers ee in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" next_ptr new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
    in
    "Collect" >::: [
      "Valid" >:: llvm_test Gc.generate test_valid
    ]
  in
  "Generated GC" >::: [
    test_initialize;
    test_malloc;
    test_collect
  ]

let suite =
  "Garbage Collector" >::: [
    test_generate
  ]
