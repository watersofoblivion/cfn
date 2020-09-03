open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  module Dwarf : sig
    val uleb128_decode : unit ptr -> Unsigned.uint64
    val sleb128_decode : unit ptr -> int64
  end

  val personality : int32 -> int32 -> Unsigned.uint64 -> unit ptr -> unit ptr -> int32
end

module Bind (Exn: Exn.Asm) (Exe: TargetTest.Exe) = struct
  module Dwarf = struct
    let uleb128_decode =
      let ty = Foreign.funptr (ptr void @-> returning uint64_t) in
      Exe.func ty Exn.Dwarf.Names.uleb128_decode

    let sleb128_decode =
      let ty = Foreign.funptr (ptr void @-> returning int64_t) in
      Exe.func ty Exn.Dwarf.Names.sleb128_decode
  end

  (* let throw =
    let ty = Foreign.funptr (ptr void @-> ptr void @-> ptr void @-> returning void) in
    Exe.func ty Exn.Names.throw *)

  let personality =
    let ty = Foreign.funptr (int32_t @-> int32_t @-> uint64_t @-> ptr void @-> ptr void @-> returning int32_t) in
    Exe.func ty Exn.Names.personality

  (* let begin_catch =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Exn.Names.begin_catch *)

  (* let end_catch =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Exn.Names.end_catch *)
end

let exn_test test_fn =
  let heap_size = 1024n in

  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in
    let module Libc = Libc.Generate (Types) (Target) in
    let module Gc = Gc.Generate (Types) (Libc) (Target) in

    let module Asm = Exn.Generate (Types) (Libc) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Gc = GcTest.Bind (Gc) (Exe) in
    let module Exn = Bind (Asm) (Exe) in

    ignore (Gc.init heap_size);
    ignore (Gc.close_perm_gen ());
    ignore (Gc.swap_spaces ());
    ignore (Gc.init_main_gen ());

    test_fn (module Gc: GcTest.Bindings) (module Exn: Bindings))

let test_dwarf =
  let test_leb =
    let test_signed =
      let test_valid _ (module Exn: Bindings) _ =
        ignore (Exn.Dwarf.sleb128_decode null)
      in
      "Signed" >::: [
        "Valid" >:: exn_test test_valid;
      ]
    in
    let test_unsigned =
      let test_valid _ (module Exn: Bindings) _ =
        ignore (Exn.Dwarf.uleb128_decode null)
      in
      "Unsigned" >::: [
        "Valid" >:: exn_test test_valid
      ]
    in
    "LEB128 Encoding" >::: [
      test_signed;
      test_unsigned
    ]
  in
  "Dwarf" >::: [
    test_leb
  ]

let test_throw =
  let test_valid (module Gc: GcTest.Bindings) (module Exn: Bindings) ctxt =
    let _ = ctxt in
    ()
  in
  "Throw" >::: [
    "Valid" >:: exn_test test_valid
  ]

let test_personality =
  let test_valid (module Gc: GcTest.Bindings) (module Exn: Bindings) ctxt =
    let _ = ctxt in
    ignore (Exn.personality 0l 0l (Unsigned.UInt64.of_int64 0L) null null)
  in
  "Personality Function" >::: [
    "Valid" >:: exn_test test_valid
  ]

let test_begin_catch =
  let test_valid (module Gc: GcTest.Bindings) (module Exn: Bindings) ctxt =
    let _ = ctxt in
    ()
  in
  "Begin Catch" >::: [
    "Valid" >:: exn_test test_valid
  ]

let test_end_catch =
  let test_valid (module Gc: GcTest.Bindings) (module Exn: Bindings) ctxt =
    let _ = ctxt in
    ()
  in
  "End Catch" >::: [
    "Valid" >:: exn_test test_valid
  ]

let suite =
  "Exception Handling" >::: [
    test_dwarf;
    test_throw;
    test_personality;
    test_begin_catch;
    test_end_catch
  ]
