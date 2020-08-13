open Cmdliner

(**
 {1 Shared CLI}
 *)

(**
 {2 Common Options}
 *)

val exits : Term.exit_info list
val verbose : bool Term.t

(**
 {2 Help Topics}
 *)

val help : string -> string -> Cmdliner.Manpage.block list -> unit Term.t * Term.info
