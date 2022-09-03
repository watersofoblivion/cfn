open Cmdliner

(**
 {1 Shared CLI}
 *)

(**
 {2 Common Options}
 *)

val exits : Term.exit_info list
(**
  The set of exit statuses.

  @since 1.0
*)

val verbose : bool Term.t
(**
  The verbose flag.

  @since 1.0
*)

(**
 {2 Help Topics}
 *)

val help : string -> string -> Cmdliner.Manpage.block list -> unit Term.t * Term.info
(**
  The [help] command.

  @since 1.0
*)
