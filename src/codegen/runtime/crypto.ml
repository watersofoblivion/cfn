open Llvm

module type Asm = sig
  module type Algorithm = sig
    module Names : sig
      val init : string
      val compress : string
      val finalize : string
    end

    val digest_len : int

    val state_t : lltype

    val init : llvalue
    val compress : llvalue
    val finalize : llvalue
  end

  module Hash : sig
    module MD5 : Algorithm
    module SHA256 : Algorithm
  end

  module HMAC : sig
    module MD5 : Algorithm
    module SHA256 : Algorithm
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

  let memset =
    let ty = function_type Types.void_t [|pointer_type Types.byte_t; Types.byte_t; Types.int_t; Types.bool_t|] in
    declare_function "llvm.memset.p0i8.i32" ty Target.md

  let rec range a b f acc =
    if a < b
    then range (a + 1) b f (f acc a)
    else acc

  let copy_state state idx builder =
    let idx = [|const_int Types.int_t idx|] in
    let ptr = build_gep state idx "" builder in
    build_load ptr "" builder

  let feedback_state state x x' idx builder =
    let x = build_add x x' "" builder in
    let ptr = build_gep state [|const_int Types.int_t idx|] "" builder in
    ignore (build_store x ptr builder)

  let _ = feedback_state

  module MD5 = struct
    let name = "md5"

    let digest_len = 4
    let state_len = 4
    let block_len = 64

    let init = [0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l]

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

    let round fn a b c d m s t builder =
      let tmp_1 = fn b c d builder in
      let tmp_2 = build_add a tmp_1 "" builder in
      let tmp_3 = build_add tmp_2 m "" builder in
      let tmp_4 = build_add tmp_3 t "" builder in
      let tmp_5 = rol tmp_4 s builder in
      build_add tmp_5 b "" builder

    let f_round = round f
    let g_round = round g
    let h_round = round h
    let i_round = round i

    let w_order =
      [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15;
        1; 6; 11; 0; 5; 10; 15; 4; 9; 14; 3; 8; 13; 2; 7; 12;
        5; 8; 11; 14; 1; 4; 7; 10; 13; 0; 3; 6; 9; 12; 15; 2;
        0; 7; 14; 5; 12; 3; 10; 1; 8; 15; 6; 13; 4; 11; 2; 9|]

    let r_order =
      Array.map (const_int Types.uint_t)
        [|7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22;
          5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20;
          4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23;
          6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21|]

    let k_order =
      Array.map (const_int Types.uint_t)
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

    let compress_block state block builder =
      let _ = block in
      let _ = builder in
      (* Pack 64-byte block into 16 32-byte integers *)
      let w =
        let mask = const_int Types.int_t 0xFF in
        let w = Array.make 16 in

        let _ = mask in

        (* let fn _ i =
          let fn x j =
            let block_idx = const_int Types.int_t (i * 4 + j) in
            let block_ptr = build_gep block [|block_idx|] "" builder in
            let block_elem = build_load block_ptr "" builder in

            let shift_amt = const_int Types.int_t (8 * j) in
            let tmp = build_shl block_elem shift_amt "" builder in

            build_or x
          in

          (* let w_ptr = in *)
          (* let w_elem = in *)
          range 0 4 fn ()
          (* w.(idx) <- w_elem *)
        in
        range 0 16 fn (); *)

        w
      in

      let _ = w in

      (* Copy hash state to temporaries *)
      let copy_state _ =
        (copy_state state 0, copy_state state 1, copy_state state 2, copy_state state 3)
      in
      let (a, b, c, d) = copy_state () in

      (* Perform rounds *)
      let rnd fn (a, b, c, d) i =
        let _ = fn in
        let _ = i in
        (* let a = fn a b c d w.(w_order.(i)) r_order.(i) k_order.(i) builder in *)
        (d, a, b, c)
      in
      let (a, b, c, d) = range  0 16 (rnd f_round) (a, b, c, d) in
      let (a, b, c, d) = range 16 32 (rnd g_round) (a, b, c, d) in
      let (a, b, c, d) = range 32 48 (rnd h_round) (a, b, c, d) in
      let (a, b, c, d) = range 48 64 (rnd i_round) (a, b, c, d) in

      let _ = a in
      let _ = b in
      let _ = c in
      let _ = d in

      (* Feedback state *)
      let (a', b', c', d') = copy_state () in
      let _ = a' in
      let _ = b' in
      let _ = c' in
      let _ = d' in
      (* ignore (feedback_state state a' a 0); *)
      (* ignore (feedback_state state b' b 1); *)
      (* ignore (feedback_state state c' c 2); *)
      (* ignore (feedback_state state d' d 3) *)
      ()

    let _ = compress_block
  end

  module SHA256 = struct
    let name = "sha256"

    let digest_len = 8
    let state_len = 4
    let block_len = 64

    let init = [0x6A09E667l; 0xBB67AE85l; 0x3C6EF372l; 0xA54FF53Al; 0x510E527Fl; 0x9B05688Cl; 0x1F83D9ABl; 0x5BE0CD19l]

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

    let _ = ch
    let _ = maj
    let _ = sigma_0
    let _ = sigma_1
    let _ = gamma_0
    let _ = gamma_1

    let k =
      Array.map (const_int Types.uint_t)
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

    let compress_block state block builder =
      let _ = state in
      let _ = block in
      let _ = builder in

      (* Copy and expand block *)
      let w =
        let ty = array_type Types.byte_t 16 in
        let w = build_alloca ty "" builder in

        (* let rng _ i =
          ignore (build_store );
        in
        range 0 16 rng () *)

        w
      in

      let _ = w in

      (* Copy state *)
      let copy_state _ =
        (
          copy_state state 0,
          copy_state state 1,
          copy_state state 2,
          copy_state state 3,
          copy_state state 4,
          copy_state state 5,
          copy_state state 6,
          copy_state state 7
        )
      in
      let (a, b, c, d, e, f, g, h) = copy_state () in

      (* Expand state *)

      (* Perform rounds *)
      let fn (a, b, c, d, e, f, g, h) _ = (h, a, b, c, d, e, f, g) in
      let (a, b, c, d, e, f, g, h) = range 0 64 fn (a, b, c, d, e, f, g, h) in
      let _ = a in
      let _ = b in
      let _ = c in
      let _ = d in
      let _ = e in
      let _ = f in
      let _ = g in
      let _ = h in

      (* Feedback state *)
      let (a', b', c', d', e', f', g', h') = copy_state () in
      let _ = a' in
      let _ = b' in
      let _ = c' in
      let _ = d' in
      let _ = e' in
      let _ = f' in
      let _ = g' in
      let _ = h' in
      (* ignore (feedback_state state a' a 0);
      ignore (feedback_state state b' b 1);
      ignore (feedback_state state c' c 2);
      ignore (feedback_state state d' d 3);
      ignore (feedback_state state e' e 4);
      ignore (feedback_state state f' f 5);
      ignore (feedback_state state g' g 6);
      ignore (feedback_state state h' h 7) *)
      ()

    let _ = compress_block
  end

  module Names = struct
    let prefix = Target.Names.prefix ^ "::crypto"
  end

  module type Algorithm = sig
    module Names : sig
      val init : string
      val compress : string
      val finalize : string
    end

    val digest_len : int

    val state_t : lltype

    val init : llvalue
    val compress : llvalue
    val finalize : llvalue
  end

  module Hash = struct
    let eight = const_int Types.int_t 8

    let zero_byte = const_int Types.byte_t 0
    let zero_int = const_int Types.int_t 0
    let zero_long = const_int Types.long_t 0

    module type Impl = sig
      val name : string

      val digest_len : int
      val state_len : int
      val block_len : int

      val init : int32 list

      (* val compress_block : llvalue -> llvalue -> llbuilder -> unit *)
    end

    module Generate (Impl: Impl) = struct
      module Names = struct
        let prefix = Names.prefix ^ "::hash::" ^ Impl.name

        let init = prefix ^ "::init"
        let compress = prefix ^ "::compress"
        let finalize = prefix ^ "::finalize"
      end

      let digest_len = Impl.digest_len

      let state_t =
        struct_type Target.ctx [|
          Types.ulong_t;
          array_type Types.uint_t Impl.state_len;
          Types.uint_t;
          array_type Types.bool_t Impl.block_len
        |]

      let len_field = const_int Types.int_t 0
      let state_field = const_int Types.int_t 1
      let curlen_field = const_int Types.int_t 2
      let buf_field = const_int Types.int_t 3

      let init =
        let fn =
          let ty = function_type Types.void_t [|pointer_type state_t|] in
          define_function Names.init ty Target.md
        in

        let state = param fn 0 in

        let block = entry_block fn in
        let builder = builder_at_end Target.ctx block in

        (* Set length to zero *)
        let len_ptr = build_gep state [|zero_long; len_field|] "" builder in
        ignore (build_store zero_long len_ptr builder);

        (* Set current length to zero *)
        let curlen_ptr = build_gep state [|zero_long; curlen_field|] "" builder in
        ignore (build_store zero_int curlen_ptr builder);

        (* Zero out the buffer *)
        let buf_ptr = build_gep state [|zero_long; buf_field|] "" builder in
        let block_len = const_int Types.int_t Impl.block_len in
        let fls = const_int Types.bool_t 0 in
        ignore (build_call memset [|buf_ptr; zero_byte; block_len; fls|] "" builder);

        (* Set the initial state *)
        let iteri i init_value =
          let idx = const_int Types.int_t i in
          let value = const_int Types.int_t (Int32.to_int init_value) in
          let state_ptr = build_gep state [|zero_long; state_field; idx|] "" builder in
          ignore (build_store value state_ptr builder)
        in
        List.iteri iteri Impl.init;

        fn

      let compress =
        let fn =
          let ty = function_type Types.void_t [|pointer_type state_t; pointer_type Types.byte_t|] in
          define_function Names.compress ty Target.md
        in

        let state_ptr = param fn 0 in
        let block_ptr = param fn 1 in

        let _ = state_ptr in
        let _ = block_ptr in

        let block = entry_block fn in
        let builder = builder_at_end Target.ctx block in

        ignore (build_ret_void builder);

        fn

      let finalize =
        let out_ty = array_type Types.byte_t digest_len in

        let fn =
          let ty = function_type Types.void_t [|pointer_type state_t; pointer_type out_ty|] in
          define_function Names.finalize ty Target.md
        in

        let state = param fn 0 in

        let entry = entry_block fn in
        let zero_pad_and_compress = append_block Target.ctx "zero-pad-and-compress" fn in
        let dont_zero_pad_and_compress = append_block Target.ctx "dont-zero-pad-and-compress" fn in
        let zero_pad = append_block Target.ctx "zero-pad" fn in
        let append_length = append_block Target.ctx "append-length" fn in
        let copy_output = append_block Target.ctx "copy-output" fn in

        (* Entry Block *)
        let builder = builder_at_end Target.ctx entry in

        let len_ptr = build_gep state [|zero_long; len_field|] "" builder in
        let curlen_ptr = build_gep state [|zero_long; curlen_field|] "" builder in
        let buf_ptr = build_gep state [|zero_long; buf_field|] "" builder in

        let curlen = build_load curlen_ptr "" builder in

        (* Extend the length of the message *)
        let extended_len =
          let curlen_bits =
            let curlen_bits = build_mul curlen eight  "" builder in
            build_bitcast curlen_bits Types.long_t "" builder
          in
          let len = build_load len_ptr "" builder in
          let extended_len = build_add len curlen_bits "" builder in
          ignore (build_store extended_len len_ptr builder);
          extended_len
        in

        let _ = extended_len in

        (* Append the '1' bit *)
        let one = const_int Types.int_t 1 in
        let high_bit_set = const_int Types.byte_t 0x80 in
        let curlen = build_add curlen one "" builder in
        let buf_ptr = build_gep buf_ptr [|one|] "" builder in
        ignore (build_store buf_ptr high_bit_set builder);

        (* Check if the buffer has room to append the length *)
        let block_len = const_int Types.int_t Impl.block_len in
        let max_len = build_sub block_len eight "" builder in
        let should_pad_and_compress = build_icmp Icmp.Ugt curlen max_len "" builder in
        ignore (build_cond_br should_pad_and_compress zero_pad_and_compress dont_zero_pad_and_compress builder);

        (* The buffer does not have room to append the length.  Zero pad until
           the buffer is full and compress. *)
        let _ =
          let builder = builder_at_end Target.ctx zero_pad_and_compress in
          ignore (build_br zero_pad builder);
        in

        (* The buffer does have room to append the length. *)
        let _ =
          let builder = builder_at_end Target.ctx dont_zero_pad_and_compress in
          ignore (build_br zero_pad builder);
        in

        (* Zero pad until we are at the position to append the length. *)
        let _ =
          let builder = builder_at_end Target.ctx zero_pad in

          let cmp = build_icmp Icmp.Ult curlen max_len "" builder in
          let _ = cmp in
          (* ignore (build_cond_br cmp pad append_length) *)
          ()
        in

        (* Append the length and perform a final compression. *)
        let _ =
          let builder = builder_at_end Target.ctx append_length in
          let _ = builder in
          ()
        in

        (* Copy the digest to the output buffer. *)
        let _ =
          let builder = builder_at_end Target.ctx copy_output in
          let _ = builder in
          ()
        in

        fn
    end

    module MD5 = Generate (MD5)
    module SHA256 = Generate (SHA256)
  end

  module HMAC = struct
    module Generate (Impl: Hash.Impl) (Hash: Algorithm) = struct
      module Names = struct
        let prefix = Names.prefix ^ "::hmac::" ^ Impl.name

        let init = prefix ^ "::init"
        let compress = prefix ^ "::compress"
        let finalize = prefix ^ "::finalize"
      end

      let digest_len = Hash.digest_len

      let state_t = Hash.state_t

      let init = const_int Types.int_t 0
      let compress = const_int Types.int_t 0
      let finalize = const_int Types.int_t 0
    end

    module MD5 = Generate (MD5) (Hash.MD5)
    module SHA256 = Generate (SHA256) (Hash.SHA256)
  end
end
