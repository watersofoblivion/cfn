(**
 * {1 Command-Line Interface}
 *
 * Contains all of the infrastructure defining the command-line interface for
 * the build tool.
 *)

open Cmdliner

val get_cmd : unit Term.t * Term.info
val fmt_cmd : unit Term.t * Term.info
val build_cmd : unit Term.t * Term.info
val package_cmd : unit Term.t * Term.info
val deploy_cmd : unit Term.t * Term.info

val default_cmd : unit Term.t * Term.info
(** [default_cmd] is the command run when no command name is given.  It displays
    a man page with usage information. *)

val cmds : (unit Term.t * Term.info) list
(** [cmds] is the list of available commands. *)
