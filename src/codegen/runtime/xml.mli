(**
 {1 XML}
 *)

 module type Asm = sig
 end

 module Generate : functor (Target: Target.Asm) -> Asm
