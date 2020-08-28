(* HTTP *)

module type Asm = sig
  module Names : sig
  end
end

module Generate (Target: Target.Asm) = struct
  module Names = struct
  end
end
