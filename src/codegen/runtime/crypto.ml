open Llvm

module type Asm = sig
  val rol : llvalue -> llvalue -> llbuilder -> llvalue
  val ror : llvalue -> llvalue -> llbuilder -> llvalue

  val process : llbuilder -> llvalue

  module MD5 : sig
    module Names : sig
      val init : string
      val process : string
      val finalize : string
    end

    val f : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val g : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val h : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val i : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue

    val ff : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val gg : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val hh : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val ii : llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llvalue -> llbuilder -> llvalue

    val state_t : lltype

    val init : llvalue
    val process : llvalue
    val finalize : llvalue
  end

  module SHA256 : sig
    module Names : sig
      val init : string
      val process : string
      val finalize : string
    end

    val ch : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue
    val maj : llvalue -> llvalue -> llvalue -> llbuilder -> llvalue

    val s : llvalue -> llvalue -> llbuilder -> llvalue
    val r : llvalue -> llvalue -> llbuilder -> llvalue

    val sigma_0 : llvalue -> llbuilder -> llvalue
    val sigma_1 : llvalue -> llbuilder -> llvalue

    val gamma_0 : llvalue -> llbuilder -> llvalue
    val gamma_1 : llvalue -> llbuilder -> llvalue

    val state_t : lltype

    val init : llvalue
    val process : llvalue
    val finalize : llvalue
  end

  module HMAC : sig
    module Names : sig
      val init : string
      val process : string
      val finalize : string
    end

    val init : llvalue
    val process : llvalue
    val finalize : llvalue
  end
end

module Generate (Types: Types.Asm) (Target: Target.Asm) = struct
  let rol a s builder =
    let bits = const_int Types.int_t 32 in
    let tmp_1 = build_sub bits s "" builder in
    let tmp_2 = build_lshr a tmp_1 "" builder in
    let tmp_3 = build_shl a s "" builder in
    build_or tmp_2 tmp_3 "" builder

  let ror x n builder =
    let bits = const_int Types.int_t 32 in
    let tmp_1 = build_sub bits n "" builder in
    let tmp_2 = build_shl x tmp_1 "" builder in
    let tmp_3 = build_lshr x n "" builder in
    build_or tmp_2 tmp_3 "" builder

  let process _ =
    const_int Types.int_t 0

  module Names = struct
    let prefix = Target.Names.prefix ^ "::crypto"
  end

  module MD5 = struct
    module Names = struct
      let prefix = Names.prefix ^ "::md5"

      let init = prefix ^ "::init"
      let process = prefix ^ "::process"
      let finalize = prefix ^ "::finalize"
    end

    let f x y z builder =
      let tmp_1 = build_xor y z "" builder in
      let tmp_2 = build_and x tmp_1 "" builder in
      build_xor z tmp_2 "" builder

    let g x y z builder =
      let tmp_1 = build_xor y x "" builder in
      let tmp_2 = build_and z tmp_1 "" builder in
      build_xor y tmp_2 "" builder

    let h x y z builder =
      let tmp_1 = build_xor x y "" builder in
      build_xor tmp_1 z "" builder

    let i x y z builder =
      let tmp_1 = build_not z "" builder in
      let tmp_2 = build_or x tmp_1 "" builder in
      build_xor y tmp_2 "" builder

    let xx fn a b c d m s t builder =
      let tmp_1 = fn b c d builder in
      let tmp_2 = build_add a tmp_1 "" builder in
      let tmp_3 = build_add tmp_2 m "" builder in
      let tmp_4 = build_add tmp_3 t "" builder in
      let tmp_5 = rol tmp_4 s builder in
      build_add tmp_5 b "" builder

    let ff = xx f
    let gg = xx g
    let hh = xx h
    let ii = xx i

    let order ty order =
      let map = const_int ty in
      Array.map map order

    let w_order =
      order Types.byte_t
        [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15;
          1; 6; 11; 0; 5; 10; 15; 4; 9; 14; 3; 8; 13; 2; 7; 12;
          5; 8; 11; 14; 1; 4; 7; 10; 13; 0; 3; 6; 9; 12; 15; 2;
          0; 7; 14; 5; 12; 3; 10; 1; 8; 15; 6; 13; 4; 11; 2; 9|]

    let r_order =
      order Types.byte_t
        [|7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22;
          5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20;
          4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23;
          6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21|]

    let k_order =
      order Types.uint_t
        [|0xd76aa478; 0xe8c7b756; 0x242070db; 0xc1bdceee; 0xf57c0faf; 0x4787c62a; 0xa8304613; 0xfd469501;
          0x698098d8; 0x8b44f7af; 0xffff5bb1; 0x895cd7be; 0x6b901122; 0xfd987193; 0xa679438e; 0x49b40821;
          0xf61e2562; 0xc040b340; 0x265e5a51; 0xe9b6c7aa; 0xd62f105d; 0x02441453; 0xd8a1e681; 0xe7d3fbc8;
          0x21e1cde6; 0xc33707d6; 0xf4d50d87; 0x455a14ed; 0xa9e3e905; 0xfcefa3f8; 0x676f02d9; 0x8d2a4c8a;
          0xfffa3942; 0x8771f681; 0x6d9d6122; 0xfde5380c; 0xa4beea44; 0x4bdecfa9; 0xf6bb4b60; 0xbebfbc70;
          0x289b7ec6; 0xeaa127fa; 0xd4ef3085; 0x04881d05; 0xd9d4d039; 0xe6db99e5; 0x1fa27cf8; 0xc4ac5665;
          0xf4292244; 0x432aff97; 0xab9423a7; 0xfc93a039; 0x655b59c3; 0x8f0ccc92; 0xffeff47d; 0x85845dd1;
          0x6fa87e4f; 0xfe2ce6e0; 0xa3014314; 0x4e0811a1; 0xf7537e82; 0xbd3af235; 0x2ad7d2bb; 0xeb86d391|]

    let _ = w_order
    let _ = r_order
    let _ = k_order

    let state_t =
      struct_type Target.ctx [|
        Types.ulong_t;
        array_type Types.uint_t 4;
        Types.uint_t;
        array_type Types.bool_t 64
      |]

    let init =
      let fn =
        let ty = function_type Types.void_t [|pointer_type state_t|] in
        define_function Names.init ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      let state_0 = const_int Types.uint_t 0x67452301 in
      let state_1 = const_int Types.uint_t 0xefcdab89 in
      let state_2 = const_int Types.uint_t 0x98badcfe in
      let state_3 = const_int Types.uint_t 0x10325476 in

      let _ = state_0 in
      let _ = state_1 in
      let _ = state_2 in
      let _ = state_3 in

      ignore (build_ret_void builder);

      fn

    let process =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.process ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn

    let finalize =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.finalize ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn
  end

  module SHA256 = struct
    module Names = struct
      let prefix = Names.prefix ^ "::sha256"

      let init = prefix ^ "::init"
      let process = prefix ^ "::process"
      let finalize = prefix ^ "::finalize"
    end

    let ch x y z builder =
      let tmp_1 = build_xor y z "" builder in
      let tmp_2 = build_and x tmp_1 "" builder in
      build_xor z tmp_2 "" builder

    let maj x y z builder =
      let tmp_1 = build_or x y "" builder in
      let tmp_2 = build_and tmp_1 z "" builder in
      let tmp_3 = build_and x y "" builder in
      build_or tmp_2 tmp_3 "" builder

    let s = ror

    let r x n builder =
      let tmp_1 = const_int Types.uint_t 0xFFFFFFFF in
      let tmp_2 = build_and x tmp_1 "" builder in
      build_ashr tmp_2 n "" builder

    let sigma a b c x builder =
      let a = const_int Types.int_t a in
      let b = const_int Types.int_t b in
      let c = const_int Types.int_t c in
      let tmp_1 = s x a builder in
      let tmp_2 = s x b builder in
      let tmp_3 = s x c builder in
      let tmp_4 = build_xor tmp_1 tmp_2 "" builder in
      build_xor tmp_4 tmp_3 "" builder

    let sigma_0 = sigma 2 13 22
    let sigma_1 = sigma 6 11 25

    let gamma a b c x builder =
      let a = const_int Types.int_t a in
      let b = const_int Types.int_t b in
      let c = const_int Types.int_t c in
      let tmp_1 = s x a builder in
      let tmp_2 = s x b builder in
      let tmp_3 = r x c builder in
      let tmp_4 = build_xor tmp_1 tmp_2 "" builder in
      build_xor tmp_4 tmp_3 "" builder

    let gamma_0 = gamma 7 18 3
    let gamma_1 = gamma 17 19 10

    let k =
      let map = const_int Types.uint_t in
      Array.map map
        [|0x428a2f98; 0x71374491; 0xb5c0fbcf; 0xe9b5dba5; 0x3956c25b;
          0x59f111f1; 0x923f82a4; 0xab1c5ed5; 0xd807aa98; 0x12835b01;
          0x243185be; 0x550c7dc3; 0x72be5d74; 0x80deb1fe; 0x9bdc06a7;
          0xc19bf174; 0xe49b69c1; 0xefbe4786; 0x0fc19dc6; 0x240ca1cc;
          0x2de92c6f; 0x4a7484aa; 0x5cb0a9dc; 0x76f988da; 0x983e5152;
          0xa831c66d; 0xb00327c8; 0xbf597fc7; 0xc6e00bf3; 0xd5a79147;
          0x06ca6351; 0x14292967; 0x27b70a85; 0x2e1b2138; 0x4d2c6dfc;
          0x53380d13; 0x650a7354; 0x766a0abb; 0x81c2c92e; 0x92722c85;
          0xa2bfe8a1; 0xa81a664b; 0xc24b8b70; 0xc76c51a3; 0xd192e819;
          0xd6990624; 0xf40e3585; 0x106aa070; 0x19a4c116; 0x1e376c08;
          0x2748774c; 0x34b0bcb5; 0x391c0cb3; 0x4ed8aa4a; 0x5b9cca4f;
          0x682e6ff3; 0x748f82ee; 0x78a5636f; 0x84c87814; 0x8cc70208;
          0x90befffa; 0xa4506ceb; 0xbef9a3f7; 0xc67178f2|]

    let _ = k

    let state_t =
      struct_type Target.ctx [|
        Types.ulong_t;
        array_type Types.uint_t 8;
        Types.uint_t;
        array_type Types.bool_t 64
      |]

    let init =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.init ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      let state_0 = const_int Types.uint_t 0x6A09E667 in
      let state_1 = const_int Types.uint_t 0xBB67AE85 in
      let state_2 = const_int Types.uint_t 0x3C6EF372 in
      let state_3 = const_int Types.uint_t 0xA54FF53A in
      let state_4 = const_int Types.uint_t 0x510E527F in
      let state_5 = const_int Types.uint_t 0x9B05688C in
      let state_6 = const_int Types.uint_t 0x1F83D9AB in
      let state_7 = const_int Types.uint_t 0x5BE0CD19 in

      let _ = state_0 in
      let _ = state_1 in
      let _ = state_2 in
      let _ = state_3 in
      let _ = state_4 in
      let _ = state_5 in
      let _ = state_6 in
      let _ = state_7 in

      ignore (build_ret_void builder);

      fn

    let process =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.process ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn

    let finalize =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.finalize ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn
  end

  module HMAC = struct
    module Names = struct
      let prefix = Names.prefix ^ "::hmac"

      let init = prefix ^ "::init"
      let process = prefix ^ "::process"
      let finalize = prefix ^ "::finalize"
    end

    let init =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.init ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn

    let process =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.process ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn

    let finalize =
      let fn =
        let ty = function_type Types.void_t [||] in
        define_function Names.finalize ty Target.md
      in

      let block = entry_block fn in
      let builder = builder_at_end Target.ctx block in

      ignore (build_ret_void builder);

      fn
  end
end
