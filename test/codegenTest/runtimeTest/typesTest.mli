open Runtime

module type Bindings = sig
  type int_t = int32
  type long_t = int64
  type word_t = nativeint
  type void_t = unit

  type void_ptr_t

  val int_to_ptr : nativeint -> void_ptr_t
  val ptr_to_int : void_ptr_t -> nativeint
end

module Bind : functor (Types: Types.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
