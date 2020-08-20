(**
 {1 Garbage Collector}
 *)

(**
 {2 Names}
 *)

val base_ptr_name : string
(** [base_ptr_name] is the name of the global variable pointing to the beginning
    of the heap. *)

val reset_ptr_name : string
(** [reset_ptr_name] is the name of the global variable pointing to the end of
    the perm generation. *)

val next_ptr_name : string
(** [next_ptr_name] is the name of the global variable pointing to the starting
    address of the next allocated block of the heap. *)

val end_ptr_name : string
(** [end_ptr_name] is the name of the global variable pointing to the end of the
    heap. *)

val from_ptr_name : string
(** [from_ptr_name] is the name of the global variable pointing to the beginning
    of the garbage collector's "from" space. *)

val to_ptr_name : string
(** [to_ptr_name] is the name of the global variable pointing to the beginning
    of the garbage collector's "to" space. *)

val init_name : string
(** [init_name] is the name of the initialization function. *)

val malloc_name : string
(** [malloc_name] is the name of the allocation function. *)

val collect_name : string
(** [collect_name] is the name of the full collection function. *)

(**
 {2 Collector}
 *)

type t
(** Garbage Collector *)

val generate : Llvm.llmodule -> t
(** [generage mod] generates the garbage collector into [mod]. *)

val base_ptr : t -> Llvm.llvalue
(** [base_ptr gc] returns the global variable which points to the beginning of
    the heap. *)

val reset_ptr : t -> Llvm.llvalue
(** [reset_ptr gc] returns the global variable which points to the end of the perm
    generation. *)

val next_ptr : t -> Llvm.llvalue
(** [next_ptr gc] returns the global variable which points to the starting address
    of the next allocated block on the heap. *)

val end_ptr : t -> Llvm.llvalue
(** [end_ptr gc] returns the global variable which points to the end of the
    heap. *)

val from_ptr : t -> Llvm.llvalue
(** [from_ptr gc] returns the global variable pointing to the beginning of the
    garbage collector's "from" space. *)

val to_ptr : t -> Llvm.llvalue
(** [to_ptr gc] returns the global variable pointing to the beginning of the
    garbage collector's "to" space. *)

val init : t -> Llvm.llvalue
(** [init gc] returns the collector initialization function. *)

val malloc : t -> Llvm.llvalue
(** [malloc gc] returns the allocation function. *)

val collect : t -> Llvm.llvalue
(** [collect gc] returns the full collection function. *)
