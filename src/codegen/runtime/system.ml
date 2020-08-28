
module type Asm = sig
  module Types : Types.Asm
  module Syscall : Syscall.Asm
  module Libc : Libc.Asm
  module Unwind : Unwind.Asm
  module Exn : Exn.Asm
  module Gc : Gc.Asm
  module Json : Json.Asm
  module Xml : Xml.Asm
  module Http : Http.Asm
end

module Generate (Target: Target.Asm) = struct
  module Types = Types.Generate (Target)
  module Syscall = Syscall.Generate (Types) (Target)
  module Libc = Libc.Generate (Types) (Target)
  module Unwind = Unwind.Generate (Types) (Target)
  module Exn = Exn.Generate (Types) (Syscall) (Unwind) (Target)
  module Gc = Gc.Generate (Types) (Libc) (Target)
  module Json = Json.Generate (Target)
  module Xml = Xml.Generate (Target)
  module Http = Http.Generate (Target)
end
