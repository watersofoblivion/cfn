open Llvm

(**
 {1 Exception Handling}
 *)

(**
 {2 Names}
 *)

val throw_name : string
(** [throw_name] is the name of the function which throws an exception. *)

val personality_name : string
(** [personality_name] is the name of the exception handling personality
    function. *)

val begin_catch_name : string
(** [begin_catch_name] is the name of the function called on entering a catch
    block. *)

val end_catch_name : string
(** [end_catch_name] is the name of the function called on exiting a catch
    block. *)

(**
 {2 Code}
 *)

type t
(** Generated exception handling code *)

val generate : Syscall.t -> Libc.t -> Unwind.t -> llmodule -> t
(** [generate syscall libc unwind md] generates exception handling functions and
    values into the LLVM module [md]. *)

val throw : t -> llvalue
(** [throw eh] returns the function which throws an exception. *)

val personality : t -> llvalue
(** [personality eh] returns the exception handling personality function. *)

val begin_catch : t -> llvalue
(** [begin_catch eh] is the function called when entering a catch block. *)

val end_catch : t -> llvalue
(** [end_catch eh] is the function called on exiting a catch block. *)
