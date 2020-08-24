open Format

open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val base_ptr : unit -> Unsigned.uint64
  val reset_ptr : unit -> Unsigned.uint64
  val next_ptr : unit -> Unsigned.uint64
  val end_ptr : unit -> Unsigned.uint64

  val from_ptr : unit -> Unsigned.uint64
  val to_ptr : unit -> Unsigned.uint64

  val gen_pointers : unit -> (Unsigned.uint64 * Unsigned.uint64 * Unsigned.uint64 * Unsigned.uint64)
  val space_pointers : unit -> (Unsigned.uint64 * Unsigned.uint64)

  val init : int64 -> unit
  val malloc : int64 -> (int64, [`C]) pointer
  val close_perm_gen : unit -> unit
  val swap_spaces : unit -> unit
  val init_main_gen : unit -> unit
  val major : unit -> unit
end

module Bind (Asm: Gc.Asm) (Exe: TargetTest.Exe) = struct
  let base_ptr _ = Exe.global Ctypes.uint64_t Asm.Names.base_ptr
  let reset_ptr _ = Exe.global Ctypes.uint64_t Asm.Names.reset_ptr
  let next_ptr _ = Exe.global Ctypes.uint64_t Asm.Names.next_ptr
  let end_ptr _ = Exe.global Ctypes.uint64_t Asm.Names.end_ptr

  let from_ptr _ = Exe.global Ctypes.uint64_t Asm.Names.from_ptr
  let to_ptr _ = Exe.global Ctypes.uint64_t Asm.Names.to_ptr

  let gen_pointers _ = (base_ptr (), reset_ptr (), next_ptr (), end_ptr ())
  let space_pointers _ = (from_ptr (), to_ptr ())

  let init =
    let ty = Foreign.funptr (int64_t @-> returning void) in
    Exe.func ty Asm.Names.init

  let malloc =
    let ty = Foreign.funptr (int64_t @-> returning (ptr int64_t)) in
    Exe.func ty Asm.Names.malloc

  let close_perm_gen =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.close_perm_gen

  let swap_spaces =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.swap_spaces

  let init_main_gen =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.init_main_gen

  let major =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.major
end

let gc_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Libc = Libc.Generate (Target) in

    let module Asm = Gc.Generate (Libc) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Gc = Bind (Asm) (Exe) in
    test_fn (module Gc: Bindings))

let test_generate =
  let printer x = sprintf "%x" (Unsigned.UInt64.to_int x) in

  let test_initialize =
    let test_valid (module Gc: Bindings) ctxt =
      let zero = Unsigned.UInt64.of_int64 (Int64.zero) in

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      assert_equal ~ctxt ~msg:"GC Base Pointer" zero base_ptr;
      assert_equal ~ctxt ~msg:"GC Reset Pointer" zero reset_ptr;
      assert_equal ~ctxt ~msg:"GC Next Pointer" zero next_ptr;
      assert_equal ~ctxt ~msg:"GC End Pointer" zero end_ptr;

      let heap_size = Int64.of_int 1024 in
      Gc.init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
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
      "Valid" >:: gc_test test_valid
    ]
  in
  let test_malloc =
    let test_valid (module Gc: Bindings) ctxt =
      let heap_size = Int64.of_int 1024 in
      Gc.init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      let size_one = Int64.of_int 8 in
      let expected_next =
        let size = Unsigned.UInt64.of_int64 size_one in
        Unsigned.UInt64.add next_ptr size
      in
      let addr_one =
        size_one
          |> Gc.malloc
          |> to_voidp
          |> raw_address_of_ptr
          |> Int64.of_nativeint
          |> Unsigned.UInt64.of_int64
      in

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
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
          |> Gc.malloc
          |> to_voidp
          |> raw_address_of_ptr
          |> Int64.of_nativeint
          |> Unsigned.UInt64.of_int64
      in

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_next new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
      assert_equal ~ctxt ~printer ~msg:"Returned Address" expected_addr addr_two
    in
    let test_invalid =
      let test_out_of_memory (module Gc: Bindings) ctxt =
        let zero = Unsigned.UInt64.of_int64 (Int64.zero) in

        let heap_size = Int64.of_int 4 in
        Gc.init heap_size;

        let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
        let addr =
          let alloc_size = Int64.of_int 8 in
          alloc_size
            |> Gc.malloc
            |> to_voidp
            |> raw_address_of_ptr
            |> Int64.of_nativeint
            |> Unsigned.UInt64.of_int64
        in

        let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
        assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" next_ptr new_next_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
        assert_equal ~ctxt ~printer ~msg:"Returned Address" zero addr;
      in
      "Invalid" >::: [
        "Out of Memory" >:: gc_test test_out_of_memory
      ]
    in
    "Malloc" >::: [
      "Valid" >:: gc_test test_valid;
      test_invalid
    ]
  in
  let test_collect =
    let heap_size = Int64.of_int 1024 in
    let padding = Int64.of_int 64 in
    let zero = Unsigned.UInt64.of_int64 (Int64.zero) in

    let test_close_perm_gen (module Gc: Bindings) ctxt =
      Gc.init heap_size;

      let (from_ptr, to_ptr) = Gc.space_pointers () in
      assert_equal ~ctxt ~printer ~msg:"From Space Pointer" zero from_ptr;
      assert_equal ~ctxt ~printer ~msg:"To Space Pointer" zero to_ptr;

      ignore (Gc.malloc padding);

      let (base_ptr, _, next_ptr, _) = Gc.gen_pointers () in
      let (from_ptr, to_ptr) = Gc.space_pointers () in
      assert_equal ~ctxt ~printer ~msg:"From Space Pointer" zero from_ptr;
      assert_equal ~ctxt ~printer ~msg:"To Space Pointer" zero to_ptr;

      Gc.close_perm_gen ();

      let (from_ptr, to_ptr) = Gc.space_pointers () in
      assert_equal ~ctxt ~printer ~msg:"From Space Pointer" base_ptr from_ptr;
      assert_equal ~ctxt ~printer ~msg:"To Space Pointer" next_ptr to_ptr
    in
    let test_swap_spaces (module Gc: Bindings) ctxt =
      Gc.init heap_size;
      ignore (Gc.malloc padding);
      Gc.close_perm_gen ();

      let (_, to_ptr) = Gc.space_pointers () in
      Gc.swap_spaces ();

      let (base_ptr, _, next_ptr, _) = Gc.gen_pointers () in
      let (new_from_ptr, new_to_ptr) = Gc.space_pointers () in
      assert_equal ~ctxt ~printer ~msg:"From Space Pointer" to_ptr new_from_ptr;
      assert_equal ~ctxt ~printer ~msg:"To Space Pointer" base_ptr new_to_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" base_ptr next_ptr
    in
    let test_init_main_gen (module Gc: Bindings) ctxt =
      Gc.init heap_size;
      ignore (Gc.malloc padding);
      Gc.close_perm_gen ();
      ignore (Gc.malloc padding);
      Gc.swap_spaces ();
      ignore (Gc.malloc padding);
      Gc.init_main_gen ();

      let (base_ptr, reset_ptr, next_ptr, _) = Gc.gen_pointers () in
      let expected_ptr =
        let size = Unsigned.UInt64.of_int64 padding in
        Unsigned.UInt64.add base_ptr size
      in
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_ptr next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" expected_ptr reset_ptr
    in
    let test_collect (module Gc: Bindings) ctxt =
      Gc.init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      let size = Int64.of_int 8 in
      let expected_next =
        let size = Unsigned.UInt64.of_int64 size in
        Unsigned.UInt64.add next_ptr size
      in
      ignore (Gc.malloc size);

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_next new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;

      Gc.major ();

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" next_ptr new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
    in
    "Collection" >::: [
      "Close Permanent Generation" >:: gc_test test_close_perm_gen;
      "Swap Spaces"                >:: gc_test test_swap_spaces;
      "Initialize Main Generation" >:: gc_test test_init_main_gen;
      "Major Collection"           >:: gc_test test_collect;
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
