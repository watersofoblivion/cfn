open Format

module type Lang =
  sig
    module Clos :
      sig
        type ty
        type t
        val pp : t -> formatter -> unit
        val check : ty Env.t -> t -> unit
        val codegen : t -> unit
      end

    module Ir :
      sig
        type ty
        type t
        val pp : t -> formatter -> unit
        val check : ty Env.t -> t -> unit
        val clos : ty Env.t -> t -> Clos.t
      end

    module Opt :
      sig
        val opt : Ir.t -> Ir.t
      end

    module Desugar :
      sig
        type ty
        type t
        val pp : t -> formatter -> unit
        val check : ty Env.t -> t -> unit
        val normalize : ty Env.t -> t -> Ir.t
      end

    module Syntax :
      sig
        type ty
        type t
        val parse : Lexing.lexbuf -> ty Env.t -> t
        val pp : t -> formatter -> unit
        val annot : ty Env.t -> t -> t
        val lint : t -> Lint.res
        val desugar : ty Env.t -> t -> Desugar.t
      end
  end

module type Cli =
  sig
    val run_args : string array -> unit
    val run : unit -> unit
  end

module Compiler =
  functor (Lang: Lang) ->
  struct
    let run_args _ = ()
    let run _ = ()
  end

module type Calculus =
  sig
    type ty
    type tm
    type top

    val parse : Lexing.lexbuf -> 'a Env.t -> ('a Env.t -> top -> 'b) -> 'b

    val pp : ?pp_sym:(Sym.t -> formatter -> unit) -> top -> formatter -> unit
    val check : ty Env.t -> top -> unit
    val lint : top -> Lint.res

    val is_value : top -> bool
    val eval1 : tm Env.t -> top -> (tm Env.t -> top -> 'a) -> 'a
  end

module Interpreter =
  functor (Calculus: Calculus) ->
  struct
    let run_args _ = ()
    let run _ = ()
  end
