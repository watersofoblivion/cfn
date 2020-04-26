(**
 * {1 Garbage Collector}
 *)

 type t = private {
   (* State *)
   base_ptr:  Llvm.llvalue; (** Base address of heap *)
   reset_ptr: Llvm.llvalue; (** Address to reset next_ptr to on collection *)
   next_ptr:  Llvm.llvalue; (** Address of the next value to be allocated *)
   end_ptr:   Llvm.llvalue; (** End address of the heap *)

   (* C Integration API *)
   malloc:              Llvm.llvalue; (** Allocate memory *)
   malloc_with_realloc: Llvm.llvalue; (** Allocate memory that can be re-allocated *)
   calloc_with_realloc: Llvm.llvalue; (** Allocate and clear memory that can be re-allocated *)
   realloc:             Llvm.llvalue; (** Re-allocate memory *)
   strdup:              Llvm.llvalue; (** Duplicate a string *)
   free:                Llvm.llvalue; (** Free memory *)

   (* Runtime API *)
   init_perm_gen:  Llvm.llvalue; (** Finalize the sys gen and initialize the perm gen *)
   close_perm_gen: Llvm.llvalue; (** Close the perm gen in preparation for stop-copy pass *)
   init_main_gen:  Llvm.llvalue; (** Initialize the main gen after the stop-copy pass *)
   collect:        Llvm.llvalue  (** Perform a full GC between requests *)
 }
 (** Garbage Collector *)

val gen : Llvm.llcontext -> Llvm.llmodule -> t
(** [gen ctx mod] generates a garbage collector into [mod]. *)
