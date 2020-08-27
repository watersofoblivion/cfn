open Runtime

module type Bindings = sig
end

module Bind : functor (Asm: Types.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
