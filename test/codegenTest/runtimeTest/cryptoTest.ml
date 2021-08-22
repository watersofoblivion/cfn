open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Crypto: Crypto.Asm) (Exe: TargetTest.Exe) = struct
end

(* let crypto_test test_fn =
  let heap_size = 4096n in

  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in
    let module Asm = Crypto.Generate (Types) (Target) in
    let module Libc = Libc.Generate (Types) (Target) in
    let module Gc = Gc.Generate (Types) (Libc) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Gc = GcTest.Bind (Gc) (Exe) in
    let module Crypto = Bind (Asm) (Exe) in

    ignore (Gc.init heap_size);
    ignore (Gc.close_perm_gen ());
    ignore (Gc.swap_spaces ());
    ignore (Gc.init_main_gen ());

    test_fn (module Gc: GcTest.Bindings) (module Crypto: Bindings))

let _ = crypto_test *)

let suite =
  (* let test_hash =
    let test_md5 (module Gc: GcTest.Bindings) (module Crypto: Bindings) _ =
      ()
    in
    let test_sha256 (module Gc: GcTest.Bindings) (module Crypto: Bindings) _ =
      ()
    in
    "Hash" >::: [
      "MD5"    >:: crypto_test test_md5;
      "SHA256" >:: crypto_test test_sha256
    ]
  in
  let test_hmac =
    let test_md5 (module Gc: GcTest.Bindings) (module Crypto: Bindings) _ =
      ()
    in
    let test_sha256 (module Gc: GcTest.Bindings) (module Crypto: Bindings) _ =
      ()
    in
    "HMAC" >::: [
      "MD5"    >:: crypto_test test_md5;
      "SHA256" >:: crypto_test test_sha256
    ]
  in *)
  "Cryptographic Functions" >::: [
    (* test_hash; *)
    (* test_hmac *)
  ]
