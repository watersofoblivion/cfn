(**
 {1 Dwarf}
 *)

 module type Asm = sig
 end

 module Generate : functor (Target: Target.Asm) -> Asm
