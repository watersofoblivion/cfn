open Runtime

module type Bindings = sig
  module Types : TypesTest.Bindings

  val exit : Types.void_t -> Types.void_t
end

module Bind : functor (Types: TypesTest.Bindings) ->
              functor (Syscall: Syscall.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
