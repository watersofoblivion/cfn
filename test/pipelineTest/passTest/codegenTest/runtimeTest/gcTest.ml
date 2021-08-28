open Format

open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val base_ptr : unit -> nativeint
  val reset_ptr : unit -> nativeint
  val next_ptr : unit -> nativeint
  val end_ptr : unit -> nativeint

  val from_ptr : unit -> nativeint
  val to_ptr : unit -> nativeint

  val gen_pointers : unit -> (nativeint * nativeint * nativeint * nativeint)
  val space_pointers : unit -> (nativeint * nativeint)

  val init : nativeint -> unit
  val malloc : nativeint -> unit Ctypes.ptr
  val close_perm_gen : unit -> unit
  val swap_spaces : unit -> unit
  val init_main_gen : unit -> unit
  val major : unit -> unit
end

module Bind (Gc: Gc.Asm) (Exe: TargetTest.Exe) = struct
  let base_ptr _ = Exe.global nativeint Gc.Names.base_ptr
  let reset_ptr _ = Exe.global nativeint Gc.Names.reset_ptr
  let next_ptr _ = Exe.global nativeint Gc.Names.next_ptr
  let end_ptr _ = Exe.global nativeint Gc.Names.end_ptr

  let from_ptr _ = Exe.global nativeint Gc.Names.from_ptr
  let to_ptr _ = Exe.global nativeint Gc.Names.to_ptr

  let gen_pointers _ = (base_ptr (), reset_ptr (), next_ptr (), end_ptr ())
  let space_pointers _ = (from_ptr (), to_ptr ())

  let init =
    let ty = Foreign.funptr (nativeint @-> returning void) in
    Exe.func ty Gc.Names.init

  let malloc =
    let ty = Foreign.funptr (nativeint @-> returning (ptr void)) in
    Exe.func ty Gc.Names.malloc

  let close_perm_gen =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Gc.Names.close_perm_gen

  let swap_spaces =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Gc.Names.swap_spaces

  let init_main_gen =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Gc.Names.init_main_gen

  let major =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Gc.Names.major
end

let gc_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in
    let module Libc = Libc.Generate (Types) (Target) in

    let module Asm = Gc.Generate (Types) (Libc) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Gc = Bind (Asm) (Exe) in
    test_fn (module Gc: Bindings))

let test_generate =
  let printer x = sprintf "%x" (Nativeint.to_int x) in

  let test_initialize =
    let test_valid (module Gc: Bindings) ctxt =
      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      assert_equal ~ctxt ~msg:"GC Base Pointer" 0n base_ptr;
      assert_equal ~ctxt ~msg:"GC Reset Pointer" 0n reset_ptr;
      assert_equal ~ctxt ~msg:"GC Next Pointer" 0n next_ptr;
      assert_equal ~ctxt ~msg:"GC End Pointer" 0n end_ptr;

      let heap_size = 1024n in
      Gc.init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      let expected_end = Nativeint.add base_ptr heap_size in
      let cmp x y = not (x = y) in
      assert_equal ~ctxt ~cmp ~msg:"GC Base Pointer" 0n base_ptr;
      assert_equal ~ctxt ~msg:"GC Reset Pointer" base_ptr reset_ptr;
      assert_equal ~ctxt ~msg:"GC Next Pointer" base_ptr next_ptr;
      assert_equal ~ctxt ~msg:"GC End Pointer" expected_end end_ptr;
    in
    "Initialize" >::: [
      "Valid" >:: gc_test test_valid
    ]
  in
  let test_malloc =
    let test_valid (module Gc: Bindings) ctxt =
      let heap_size = 1024n in
      Gc.init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      let size_one = 8n in
      let expected_next = Nativeint.add next_ptr size_one in
      let addr_one =
        size_one
          |> Gc.malloc
          |> raw_address_of_ptr
      in

      let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
      assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_next new_next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
      assert_equal ~ctxt ~printer ~msg:"Returned Address" next_ptr addr_one;

      let size_two = 16n in
      let expected_addr = new_next_ptr in
      let expected_next = Nativeint.add expected_next size_two in
      let addr_two =
        size_two
          |> Gc.malloc
          |> raw_address_of_ptr
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
        let heap_size = 4n in
        Gc.init heap_size;

        let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
        let addr =
          let alloc_size = 8n in
          alloc_size
            |> Gc.malloc
            |> raw_address_of_ptr
        in

        let (new_base_ptr, new_reset_ptr, new_next_ptr, new_end_ptr) = Gc.gen_pointers () in
        assert_equal ~ctxt ~printer ~msg:"GC Base Pointer" base_ptr new_base_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" reset_ptr new_reset_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" next_ptr new_next_ptr;
        assert_equal ~ctxt ~printer ~msg:"GC End Pointer" end_ptr new_end_ptr;
        assert_equal ~ctxt ~printer ~msg:"Returned Address" 0n addr;
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
    let heap_size = 1024n in
    let padding = 64n in

    let test_close_perm_gen (module Gc: Bindings) ctxt =
      Gc.init heap_size;

      let (from_ptr, to_ptr) = Gc.space_pointers () in
      assert_equal ~ctxt ~printer ~msg:"From Space Pointer" 0n from_ptr;
      assert_equal ~ctxt ~printer ~msg:"To Space Pointer" 0n to_ptr;

      ignore (Gc.malloc padding);

      let (base_ptr, _, next_ptr, _) = Gc.gen_pointers () in
      let (from_ptr, to_ptr) = Gc.space_pointers () in
      assert_equal ~ctxt ~printer ~msg:"From Space Pointer" 0n from_ptr;
      assert_equal ~ctxt ~printer ~msg:"To Space Pointer" 0n to_ptr;

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
      let expected_ptr = Nativeint.add base_ptr padding in
      assert_equal ~ctxt ~printer ~msg:"GC Next Pointer" expected_ptr next_ptr;
      assert_equal ~ctxt ~printer ~msg:"GC Reset Pointer" expected_ptr reset_ptr
    in
    let test_collect (module Gc: Bindings) ctxt =
      Gc.init heap_size;

      let (base_ptr, reset_ptr, next_ptr, end_ptr) = Gc.gen_pointers () in
      let size = 8n in
      let expected_next = Nativeint.add next_ptr size in
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
