open Runtime

module type Bindings = sig
end

module Bind : functor (Dwarf: Dwarf.Asm) -> functor (Exe: TargetTest.Exe) -> Bindings

val suite : OUnit2.test
