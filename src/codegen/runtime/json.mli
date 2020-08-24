(**
 {1 JSON}
 *)

 module type Asm = sig
 end

 module Generate : functor (Target: Target.Asm) -> Asm
