open Cmdliner

(**
 * {1 Common Options}
 *)

val exits : Term.exit_info list
val verbose : bool Term.t
val import_path : string Term.t
