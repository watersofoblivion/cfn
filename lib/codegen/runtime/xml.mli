(**
 {1 XML}
 *)

module type Asm = sig
  module Names : sig
  end
end

module Generate : functor (Target: Target.Asm) -> Asm
