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
(** [major_name] is the name of the full major collection function. *)

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

val close_perm_gen : t -> Llvm.llvalue
(** [close_perm_gen gc] returns the function to close the permanent generation
    in preparation for stop-copy garbage collection. *)

val swap_spaces : t -> Llvm.llvalue
(** [swap_spaces gc] returns the function to swap the from and to spaces between
    stop-copy garbage collection passes. *)

val init_main_gen : t -> Llvm.llvalue
(** [init_main_gen gc] returns the function to initialize the main generation in
    preparation for event handling. *)

val major : t -> Llvm.llvalue
(** [major gc] returns the function to perform a full major collection of the
    main generation. *)
