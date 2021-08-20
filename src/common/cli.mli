open Format

module type Cli =
  sig
    val run_args : string array -> unit
    (** [run_args args] runs the compiler with the command-line arguments
        [args]. *)

    val run : unit -> unit
    (** [run ()] runs the compiler with the system command-line arguments. *)
  end
(** A command-line executable *)

module type Lang =
  sig
    module Clos :
      sig
        type ty
        (** A type *)

        type t
        (** A source file *)

        val pp : t -> formatter -> unit
        (** [pp file fmt] pretty-prints the source file [file] to the formatter
            [fmt]. *)

        val check : ty Env.t -> t -> unit
        (** [check env file] type-checks the source file [file] in the
            environment [env]. *)

        val codegen : t -> unit
        (** [codegen file] generates LLVM assembly from the source file [file]. *)
      end
    (** Closure-converted form of the language. *)

    module Ir :
      sig
        type ty
        (** A type *)

        type t
        (** A source file *)

        val pp : t -> formatter -> unit
        (** [pp file fmt] pretty-prints the source file [file] to the formatter
            [fmt]. *)

        val check : ty Env.t -> t -> unit
        (** [check env file] type-checks the source file [file] in the
            environment [env]. *)

        val clos : ty Env.t -> t -> Clos.t
        (** [clos env file] closure-converts the source file [file] in the
            environment [env]. *)
      end
    (** Intermediate representation of the language. *)

    module Opt :
      sig
        val opt : Ir.t -> Ir.t
        (** [opt file] optimizes the source file [file]. *)
      end
    (** Optimizer *)

    module Desugar :
      sig
        type ty
        (** A type *)

        type t
        (** A source file *)

        val pp : t -> formatter -> unit
        (** [pp file fmt] pretty-prints the source file [file] to the formatter
            [fmt]. *)

        val check : ty Env.t -> t -> unit
        (** [check env file] type-checks the source file [file] in the
            environment [env]. *)

        val normalize : ty Env.t -> t -> Ir.t
        (** [normalize env file] normalizes the the source file [file] in the
            environment [env] and produces an administrative normal form
            representation suitable for optimization and closure conversion. *)
      end
    (** Desugared representation of the language *)

    module Syntax :
      sig
        type ty
        (** A type *)

        type t
        (** A source file *)

        val parse : Lexing.lexbuf -> ty Env.t -> t
        (** [parse lexbuf] parses a source file from the lexing buffer [lexbuf]. *)

        val pp : t -> formatter -> unit
        (** [pp file fmt] pretty-prints the source file [file] to the formatter
            [fmt]. *)

        val annot : ty Env.t -> t -> t
        (** [annot file] type-checks the source file [file] and produces an
            annotated syntax tree. *)

        val lint : t -> Lint.res
        (** [lint env top] lints the top-level term [top] in the environment
            [env]. *)

        val desugar : ty Env.t -> t -> Desugar.t
        (** [desugar file] desugars the source file [file] and produces a
            desugared file. *)
      end
    (** Abstract syntax of the language *)
  end
(* A language that can be compiled *)

module Compiler : functor (Lang: Lang) -> Cli
(** Construct a commane-line compiler from a language implementation. *)

module type Calculus =
  sig
    type ty
    (* A type in the calculus *)

    type tm
    (** A term in the calculus *)

    type top
    (* A top-level term in the calculus *)

    (**
     * {1 Parsing}
     *)

    val parse : Lexing.lexbuf -> 'a Env.t -> ('a Env.t -> top -> 'b) -> 'b
    (** [parse lex lexbuf env kontinue] parses a top-level term of the
        calculus from the lexing buffer [lexbuf] using the lexer [lex].  The
        term is alpha-renamed in the environment [env].  A (possibly
        updated) environment and the parsed term are passed to the
        continuation [kontinue]. *)

    (**
     * {2 Operations}
     *)

    val pp : ?pp_sym:(Sym.t -> formatter -> unit) -> top -> formatter -> unit
    (** [pp top fmt] pretty-prints the top-level term [top] to the formatter
        [fmt]. *)

    val check : ty Env.t -> top -> unit
    (** [check env top] type-checks the top-level term [top] in the environment
        [env]. *)

    val lint : top -> Lint.res
    (** [lint env top] lints the top-level term [top] in the environment [env]. *)

    (**
     * {2 Evaluation}
     *)

    val is_value : top -> bool
    (** [is_value top] tests if the top-level term [top] is a value. *)

    val eval1 : tm Env.t -> top -> (tm Env.t -> top -> 'a) -> 'a
    (** [eval1 env top kontinue] performs a single evaluation step of the
        top-level term [top] in the environment [env].  A (possibly updated)
        environment and the result of the evaluation step are passed to the
        continuation [kontinue]. *)
  end
(** A calculus that can be interpreted *)

module Interpreter : functor (Calculus: Calculus) -> Cli
(** Construct a commane-line interpreter REPL from a calculus. *)
