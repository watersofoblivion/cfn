open Runtime

module type Bindings = sig
  module Types : TypesTest.Bindings
  module Libc : LibcTest.Bindings
  module Exn : ExnTest.Bindings
  module Gc : GcTest.Bindings
  module Json : JsonTest.Bindings
  module Xml : XmlTest.Bindings
  module Http : HttpTest.Bindings
end

module Bind : functor (System: System.Asm) ->
              functor (Exe: TargetTest.Exe) ->
              Bindings

val suite : OUnit2.test
