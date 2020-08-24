
module type Asm = sig
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
  module Syscall = Syscall.Generate (Target)
  module Libc = Libc.Generate (Target)
  module Unwind = Unwind.Generate (Libc) (Target)
  module Exn = Exn.Generate (Syscall) (Libc) (Unwind) (Target)
  module Gc = Gc.Generate (Libc) (Target)
  module Json = Json.Generate (Target)
  module Xml = Xml.Generate (Target)
  module Http = Http.Generate (Target)
end
