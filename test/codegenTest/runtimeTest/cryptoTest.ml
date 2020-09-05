open Llvm
open Ctypes

open Runtime

open OUnit2

module type Asm = sig
  module Names : sig
    val rol : string
    val ror : string

    val process : string
  end

  module MD5 : sig
    module Names : sig
      val f : string
      val g : string
      val h : string
      val i : string

      val ff : string
      val gg : string
      val hh : string
      val ii : string
    end
  end

  module SHA256 : sig
    module Names : sig
      val ch : string
      val maj : string
      val s : string
      val r : string
      val sigma_0 : string
      val sigma_1 : string
      val gamma_0 : string
      val gamma_1 : string
    end
  end
end

module Generate (Crypto: Crypto.Asm) (Types: Types.Asm) (Target: Target.Asm) = struct
  let wrap n name body_fn =
    let args = Array.make n Types.int_t in
    let fn =
      let ty = function_type Types.int_t args in
      define_function name ty Target.md
    in
    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in
    let res = body_fn fn builder in
    ignore (build_ret res builder)

  let wrap_1 name crypto_fn =
    wrap 1 name (fun fn builder ->
      let param_1 = param fn 0 in
      crypto_fn param_1 builder
    )

  let wrap_2 name crypto_fn =
    wrap 2 name (fun fn builder ->
      let param_1 = param fn 0 in
      let param_2 = param fn 1 in
      crypto_fn param_1 param_2 builder
    )

  let wrap_3 name crypto_fn =
    wrap 3 name (fun fn builder ->
      let param_1 = param fn 0 in
      let param_2 = param fn 1 in
      let param_3 = param fn 2 in
      crypto_fn param_1 param_2 param_3 builder
    )

  let wrap_7 name crypto_fn =
    wrap 7 name (fun fn builder ->
      let param_1 = param fn 0 in
      let param_2 = param fn 1 in
      let param_3 = param fn 2 in
      let param_4 = param fn 3 in
      let param_5 = param fn 4 in
      let param_6 = param fn 5 in
      let param_7 = param fn 6 in
      crypto_fn param_1 param_2 param_3 param_4 param_5 param_6 param_7 builder
    )

  module Names = struct
    let prefix = Target.Names.prefix ^ "::crypto-helpers"

    let rol = prefix ^ "::rol"
    let ror = prefix ^ "::ror"

    let process = prefix ^ "::process"
  end

  let _ =
    wrap_2 Names.rol Crypto.rol;
    wrap_2 Names.ror Crypto.ror

  let _ =
    let fn =
      let ty = function_type Types.int_t [||] in
      define_function Names.process ty Target.md
    in
    let block = entry_block fn in
    let builder = builder_at_end Target.ctx block in
    let res = Crypto.process builder in
    ignore (build_ret res builder)

  module MD5 = struct
    module Names = struct
      let prefix = Names.prefix ^ "::md5"

      let f = prefix ^ "::f"
      let g = prefix ^ "::g"
      let h = prefix ^ "::h"
      let i = prefix ^ "::i"

      let ff = prefix ^ "::ff"
      let gg = prefix ^ "::gg"
      let hh = prefix ^ "::hh"
      let ii = prefix ^ "::ii"
    end

    let _ =
      wrap_3 Names.f Crypto.MD5.f;
      wrap_3 Names.g Crypto.MD5.g;
      wrap_3 Names.h Crypto.MD5.h;
      wrap_3 Names.i Crypto.MD5.i;

      wrap_7 Names.ff Crypto.MD5.ff;
      wrap_7 Names.gg Crypto.MD5.gg;
      wrap_7 Names.hh Crypto.MD5.hh;
      wrap_7 Names.ii Crypto.MD5.ii
  end

  module SHA256 = struct
    module Names = struct
      let prefix = Names.prefix ^ "::sha256"

      let ch = prefix ^ "::ch"
      let maj = prefix ^ "::maj"
      let s = prefix ^ "::s"
      let r = prefix ^ "::r"
      let sigma_0 = prefix ^ "::sigma-0"
      let sigma_1 = prefix ^ "::sigma-1"
      let gamma_0 = prefix ^ "::gamma-0"
      let gamma_1 = prefix ^ "::gamma-1"
    end

    let _ =
      wrap_3 Names.ch Crypto.SHA256.ch;
      wrap_3 Names.maj Crypto.SHA256.maj;
      wrap_2 Names.s Crypto.SHA256.s;
      wrap_2 Names.r Crypto.SHA256.r;
      wrap_1 Names.sigma_0 Crypto.SHA256.sigma_0;
      wrap_1 Names.sigma_1 Crypto.SHA256.sigma_1;
      wrap_1 Names.gamma_0 Crypto.SHA256.gamma_0;
      wrap_1 Names.gamma_1 Crypto.SHA256.gamma_1
  end
end

module type Helpers = sig
  val rol : int32 -> int32 -> int32
  val ror : int32 -> int32 -> int32

  val process : unit -> int32

  module MD5 : sig
    val f : int32 -> int32 -> int32 -> int32
    val g : int32 -> int32 -> int32 -> int32
    val h : int32 -> int32 -> int32 -> int32
    val i : int32 -> int32 -> int32 -> int32

    val ff : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32
    val gg : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32
    val hh : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32
    val ii : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32
  end

  module SHA256 : sig
    val ch : int32 -> int32 -> int32 -> int32
    val maj : int32 -> int32 -> int32 -> int32

    val s : int32 -> int32 -> int32
    val r : int32 -> int32 -> int32

    val sigma_0 : int32 -> int32
    val sigma_1 : int32 -> int32

    val gamma_0 : int32 -> int32
    val gamma_1 : int32 -> int32
  end
end

module Help (Helpers: Asm) (Exe: TargetTest.Exe) = struct
  let bind ty name =
    let ty = Foreign.funptr ty in
    Exe.func ty name

  let bind_0 = bind (void @-> returning int32_t)
  let bind_1 = bind (int32_t @-> returning int32_t)
  let bind_2 = bind (int32_t @-> int32_t @-> returning int32_t)
  let bind_3 = bind (int32_t @-> int32_t @-> int32_t @-> returning int32_t)
  let bind_7 = bind (int32_t @-> int32_t @-> int32_t @-> int32_t @-> int32_t @-> int32_t @-> int32_t @-> returning int32_t)

  let rol = bind_2 Helpers.Names.rol
  let ror = bind_2 Helpers.Names.ror

  let process = bind_0 Helpers.Names.process

  module MD5 = struct
    let f = bind_3 Helpers.MD5.Names.f
    let g = bind_3 Helpers.MD5.Names.g
    let h = bind_3 Helpers.MD5.Names.h
    let i = bind_3 Helpers.MD5.Names.i

    let ff = bind_7 Helpers.MD5.Names.ff
    let gg = bind_7 Helpers.MD5.Names.gg
    let hh = bind_7 Helpers.MD5.Names.hh
    let ii = bind_7 Helpers.MD5.Names.ii
  end

  module SHA256 = struct
    let ch = bind_3 Helpers.SHA256.Names.ch
    let maj = bind_3 Helpers.SHA256.Names.maj
    let s = bind_2 Helpers.SHA256.Names.s
    let r = bind_2 Helpers.SHA256.Names.r

    let sigma_0 = bind_1 Helpers.SHA256.Names.sigma_0
    let sigma_1 = bind_1 Helpers.SHA256.Names.sigma_1
    let gamma_0 = bind_1 Helpers.SHA256.Names.gamma_0
    let gamma_1 = bind_1 Helpers.SHA256.Names.gamma_1
  end
end

module type Bindings = sig
end

module Bind (Crypto: Crypto.Asm) (Exe: TargetTest.Exe) = struct
end

let crypto_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in
    let module Asm = Crypto.Generate (Types) (Target) in
    let module Helpers = Generate (Asm) (Types) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Helpers = Help (Helpers) (Exe) in
    let module Crypto = Bind (Asm) (Exe) in

    test_fn (module Helpers: Helpers) (module Crypto: Bindings))

let _ = crypto_test

let suite =
  let assert_2 ~ctxt expected actual cases =
    let iter (x, y) =
      let expected = expected x y in
      let actual = actual x y in
      assert_equal ~ctxt expected actual
    in
    List.iter iter cases
  in
  let assert_3 ~ctxt expected actual cases =
    let iter (x, y, z) =
      let expected = expected x y z in
      let actual = actual x y z in
      assert_equal ~ctxt expected actual
    in
    List.iter iter cases
  in

  let test_helpers =
    let test_rol (module Helpers: Helpers) _ ctxt =
      Helpers.rol 0x000000FFl 8l
        |> assert_equal ~ctxt 0x0000FF00l;

      Helpers.rol 0xFF000000l 4l
        |> assert_equal ~ctxt 0xF000000Fl
    in
    let test_ror (module Helpers: Helpers) _ ctxt =
      Helpers.ror 0xFF000000l 8l
        |> assert_equal ~ctxt 0x00FF0000l;

      Helpers.ror 0x000000FFl 4l
        |> assert_equal ~ctxt 0xF000000Fl
    in
    let test_process (module Helpers: Helpers) _ _ =
      ignore (Helpers.process ())
    in
    "Helpers" >::: [
      "Rotate Left"  >:: crypto_test test_rol;
      "Rotate Right" >:: crypto_test test_ror;
      "Process"      >:: crypto_test test_process;
    ]
  in
  let test_md5 =
    let test_helpers =
      let assert_xx ~ctxt fn_rol fn_x fn_xx cases =
        let iter (a, b, c, d, m, s, t) =
          let expected =
            let tmp = fn_x b c d |> Int32.add a |> Int32.add m |> Int32.add t in
            fn_rol tmp s |> Int32.add b
          in
          let actual = fn_xx a b c d m s t in
          assert_equal ~ctxt expected actual
        in
        List.iter iter cases
      in

      let test_f (module Helpers: Helpers) _ ctxt =
        let expected x y z = z |> Int32.logxor y |> Int32.logand x |> Int32.logxor z in
        assert_3 ~ctxt expected Helpers.MD5.f [(1l, 2l, 3l)]
      in
      let test_g (module Helpers: Helpers) _ ctxt =
        let expected x y z = x |> Int32.logxor y |> Int32.logand z |> Int32.logxor y in
        assert_3 ~ctxt expected Helpers.MD5.g [(1l, 2l, 3l)]
      in
      let test_h (module Helpers: Helpers) _ ctxt =
        let expected x y z = Int32.logxor (Int32.logxor x y) z in
        assert_3 ~ctxt expected Helpers.MD5.h [(1l, 2l, 3l)]
      in
      let test_i (module Helpers: Helpers) _ ctxt =
        let expected x y z = z |> Int32.lognot |> Int32.logor x |> Int32.logxor y in
        assert_3 ~ctxt expected Helpers.MD5.i [(1l, 2l, 3l)]
      in

      let test_ff (module Helpers: Helpers) _ ctxt =
        assert_xx ~ctxt
          Helpers.rol
          Helpers.MD5.f
          Helpers.MD5.ff
          [(1l, 2l, 3l, 4l, 5l, 6l, 7l)]
      in
      let test_gg (module Helpers: Helpers) _ ctxt =
        assert_xx ~ctxt
          Helpers.rol
          Helpers.MD5.g
          Helpers.MD5.gg
          [(1l, 2l, 3l, 4l, 5l, 6l, 7l)]
      in
      let test_hh (module Helpers: Helpers) _ ctxt =
        assert_xx ~ctxt
          Helpers.rol
          Helpers.MD5.h
          Helpers.MD5.hh
          [(1l, 2l, 3l, 4l, 5l, 6l, 7l)]
      in
      let test_ii (module Helpers: Helpers) _ ctxt =
        assert_xx ~ctxt
          Helpers.rol
          Helpers.MD5.i
          Helpers.MD5.ii
          [(1l, 2l, 3l, 4l, 5l, 6l, 7l)]
      in
      "Helpers" >::: [
        "F"  >:: crypto_test test_f;
        "G"  >:: crypto_test test_g;
        "H"  >:: crypto_test test_h;
        "I"  >:: crypto_test test_i;
        "FF" >:: crypto_test test_ff;
        "GG" >:: crypto_test test_gg;
        "HH" >:: crypto_test test_hh;
        "II" >:: crypto_test test_ii;
      ]
    in
    let test_init _ _ _ = () in
    let test_process _ _ _ = () in
    let test_finalize _ _ _ = () in
    "MD5" >::: [
      test_helpers;
      "Init"     >:: crypto_test test_init;
      "Process"  >:: crypto_test test_process;
      "Finalize" >:: crypto_test test_finalize;
    ]
  in
  let test_sha256 =
    let test_helpers =
      let assert_greek ~ctxt f x f' x' f'' x'' fn values =
        let iter v =
          let expected =
            let tmp_1 = f v x in
            let tmp_2 = f' v x' in
            let tmp_3 = f'' v x'' in
            let tmp_4 = Int32.logxor tmp_1 tmp_2 in
            Int32.logxor tmp_4 tmp_3
          in
          let actual = fn v in
          assert_equal ~ctxt expected actual
        in
        List.iter iter values
      in

      let test_ch (module Helpers: Helpers) _ ctxt =
        let expected x y z = z |> Int32.logxor y |> Int32.logand x |> Int32.logxor z in
        assert_3 ~ctxt expected Helpers.SHA256.ch [(1l, 2l, 3l)]
      in
      let test_maj (module Helpers: Helpers) _ ctxt =
        let expected x y z =
          let tmp =
            let tmp = Int32.logor x y in
            Int32.logand tmp z
          in
          y |> Int32.logand x |> Int32.logor tmp
        in
        assert_3 ~ctxt expected Helpers.SHA256.maj [(1l, 2l, 3l)]
      in
      let test_s (module Helpers: Helpers) _ ctxt =
        assert_2 ~ctxt Helpers.ror Helpers.SHA256.s [(1l, 2l)]
      in
      let test_r (module Helpers: Helpers) _ ctxt =
        let expected x y =
          let tmp = Int32.logand x 0xFFFFFFFFl in
          Int32.shift_right_logical tmp (Int32.to_int y)
        in
        assert_2 ~ctxt expected Helpers.SHA256.r [(1l, 2l)]
      in
      let test_sigma_0 (module Helpers: Helpers) _ ctxt =
        assert_greek ~ctxt
          Helpers.SHA256.s 2l
          Helpers.SHA256.s 13l
          Helpers.SHA256.s 22l
          Helpers.SHA256.sigma_0
          [1l]
      in
      let test_sigma_1 (module Helpers: Helpers) _ ctxt =
        assert_greek ~ctxt
          Helpers.SHA256.s 6l
          Helpers.SHA256.s 11l
          Helpers.SHA256.s 25l
          Helpers.SHA256.sigma_1
          [1l]
      in
      let test_gamma_0 (module Helpers: Helpers) _ ctxt =
        assert_greek ~ctxt
          Helpers.SHA256.s 7l
          Helpers.SHA256.s 18l
          Helpers.SHA256.r 3l
          Helpers.SHA256.gamma_0
          [1l]
      in
      let test_gamma_1 (module Helpers: Helpers) _ ctxt =
        assert_greek ~ctxt
          Helpers.SHA256.s 17l
          Helpers.SHA256.s 19l
          Helpers.SHA256.r 10l
          Helpers.SHA256.gamma_1
          [1l]
      in
      "Helpers" >::: [
        "Ch"      >:: crypto_test test_ch;
        "Maj"     >:: crypto_test test_maj;
        "S"       >:: crypto_test test_s;
        "R"       >:: crypto_test test_r;
        "Sigma-0" >:: crypto_test test_sigma_0;
        "Sigma-1" >:: crypto_test test_sigma_1;
        "Gamma-0" >:: crypto_test test_gamma_0;
        "Gamma-1" >:: crypto_test test_gamma_1;
      ]
    in
    let test_init _ _ _ = () in
    let test_process _ _ _ = () in
    let test_finalize _ _ _ = () in
    "SHA256" >::: [
      test_helpers;
      "Init"     >:: crypto_test test_init;
      "Process"  >:: crypto_test test_process;
      "Finalize" >:: crypto_test test_finalize;
    ]
  in
  let test_hmac =
    "HMAC" >::: [

    ]
  in
  "Cryptographic Functions" >::: [
    test_helpers;
    test_md5;
    test_sha256;
    test_hmac
  ]
