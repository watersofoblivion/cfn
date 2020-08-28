open Llvm

(**
 {1 Garbage Collector}
 *)

(**
 {2 Names}
 *)

(* val base_ptr_name : string
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

val close_perm_gen_name : string
(** [close_perm_gen_name] is the name of the function to close the permanent
    generation in preparation for stop-copy garbage collection. *)

val swap_spaces_name : string
(** [swap_spaces gc] is the name of the function to swap the from and to spaces
    between stop-copy garbage collection passes. *)

val init_main_gen_name : string
(** [init_main_gen gc] is the name of the function to initialize the main
    generation in preparation for event handling. *)

val malloc_name : string
(** [malloc_name] is the name of the allocation function. *)

val major_name : string
(** [major_name] is the name of the full major collection function. *) *)

(**
 {2 Collector}
 *)
(*
type t
(** Garbage Collector *)

val generate : Libc.t -> llmodule -> t
(** [generage mod] generates the garbage collector into [mod]. *)

val base_ptr : t -> llvalue
(** [base_ptr gc] returns the global variable which points to the beginning of
    the heap. *)

val reset_ptr : t -> llvalue
(** [reset_ptr gc] returns the global variable which points to the end of the perm
    generation. *)

val next_ptr : t -> llvalue
(** [next_ptr gc] returns the global variable which points to the starting address
    of the next allocated block on the heap. *)

val end_ptr : t -> llvalue
(** [end_ptr gc] returns the global variable which points to the end of the
    heap. *)

val from_ptr : t -> llvalue
(** [from_ptr gc] returns the global variable pointing to the beginning of the
    garbage collector's "from" space. *)

val to_ptr : t -> llvalue
(** [to_ptr gc] returns the global variable pointing to the beginning of the
    garbage collector's "to" space. *)

val init : t -> llvalue
(** [init gc] returns the collector initialization function. *)

val malloc : t -> llvalue
(** [malloc gc] returns the allocation function. *)

val close_perm_gen : t -> llvalue
(** [close_perm_gen gc] returns the function to close the permanent generation
    in preparation for stop-copy garbage collection. *)

val swap_spaces : t -> llvalue
(** [swap_spaces gc] returns the function to swap the from and to spaces between
    stop-copy garbage collection passes. *)

val init_main_gen : t -> llvalue
(** [init_main_gen gc] returns the function to initialize the main generation in
    preparation for event handling. *)

val major : t -> llvalue
(** [major gc] returns the function to perform a full major collection of the
    main generation. *) *)

module type Asm = sig
  module Names : sig
    val base_ptr : string
    val reset_ptr : string
    val next_ptr : string
    val end_ptr : string

    val from_ptr : string
    val to_ptr : string

    val init : string
    val malloc : string
    val close_perm_gen : string
    val swap_spaces : string
    val init_main_gen : string
    val major : string
  end

  val base_ptr : llvalue
  val reset_ptr : llvalue
  val next_ptr : llvalue
  val end_ptr : llvalue

  val from_ptr : llvalue
  val to_ptr : llvalue

  val init : llvalue
  val malloc : llvalue
  val close_perm_gen : llvalue
  val swap_spaces : llvalue
  val init_main_gen : llvalue
  val major : llvalue
end

module Generate : functor (Types: Types.Asm) -> functor (Libc: Libc.Asm) -> functor (Target: Target.Asm) -> Asm
