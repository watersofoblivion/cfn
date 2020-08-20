(**
 {1 Runtime}
 *)

type t
(** The generated runtime. *)

val generate : Llvm.llmodule -> t
(** [generate md] generates the runtime into [md]. *)

val gc : t -> Gc.t
(** [gc rt] gets the generated garbage collector. *)

val json : t -> Json.t
(** [json rt] gets the generated JSON processor. *)

val xml : t -> Xml.t
(** [xml rt] gets the generated XML processor. *)

val http : t -> Http.t
(** [http rt] gets the generated HTTP client. *)
