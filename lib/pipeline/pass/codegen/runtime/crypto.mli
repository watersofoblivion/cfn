open Llvm

module type Asm = sig
  module type Algorithm = sig
    module Names : sig
      val init : string
      (** [init] is the name of the initialization function. *)

      val compress : string
      (** [compress] is the name of the compression function. *)

      val finalize : string
      (** [finalize] is the name of the finalization function. *)
    end
    (** LLVM names of the generated functions *)

    val digest_len : int
    (** [digest_len] is the length of the digest generated by this algorithm, in
        bytes. *)

    val state_t : lltype
    (** [state_t] is the struct containing the state required for the algorithm. *)

    val init : llvalue
    (** [init] is a function which initializes a value of type {!state_t} so
        that it can be used to hash a value.  The generated function takes a
        pointer to a value of type {!state_t} and returns void. *)

    val compress : llvalue
    (** [compress] is a function which compresses a block of data into the hash.
        The generated function takes a pointer to an initialized value of type
        {!state_t} and a byte array of size [block_len] and returns void. *)

    val finalize : llvalue
    (** [finalize] is a function which finalizes a hash and computes the digest.
        The generated function takes a pointer to a {!state_t} and a pointer to
        a byte array of length {!digest_len} to populate with the digest and
        returns void. *)
  end
  (** A complete implementation of a hashing algorithm. *)

  module Hash : sig
    module MD5 : Algorithm
    module SHA256 : Algorithm
  end
  (** Available hashing algorithms *)

  module HMAC : sig
    module MD5 : Algorithm
    module SHA256 : Algorithm
  end
  (** Available HMACs *)
end

module Generate : functor (Types: Types.Asm) ->
                  functor (Target: Target.Asm) ->
                  Asm
