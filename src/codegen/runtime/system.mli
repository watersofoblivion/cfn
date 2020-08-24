(**
 {1 Runtime}
 *)
(*
type t
(** The generated runtime. *)

val generate : Llvm.llmodule -> t
(** [generate md] generates the runtime into [md]. *)

val syscall : t -> Syscall.t
(** [syscall rt] gets the syscall bindings. *)

val libc : t -> Libc.t
(** [libc rt] gets the libc bindings. *)

val unwind : t -> Unwind.t
(** [unwind rt] gets the stack unwinder. *)

val exn : t -> Exn.t
(** [exn rt] gets the exception handling functions. *)

val gc : t -> Gc.t
(** [gc rt] gets the generated garbage collector. *)

val json : t -> Json.t
(** [json rt] gets the generated JSON processor. *)

val xml : t -> Xml.t
(** [xml rt] gets the generated XML processor. *)

val http : t -> Http.t
(** [http rt] gets the generated HTTP client. *) *)

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

module Generate : functor (Target: Target.Asm) -> Asm
