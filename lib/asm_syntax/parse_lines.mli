(** Statements and expressions from tokens, through the Menhir grammar, which
    stays private to the library. *)

open Rivet_foundation

val ensure_terminated : Token.t list -> Token.t list
val error_span : Token.t list -> Span.t option
val dummy_lexbuf : unit -> Lexing.lexbuf

type error_kind =
  [ `Empty_expression | `Malformed_expression of Token.slice | `Unexpected_token of Token.t option ]

val pp_error : Format.formatter -> error_kind -> unit
val error_code : error_kind -> string
val parse_diagnostic : span:Span.t -> error_kind -> Diagnostic.t

val parse_program :
  profile:Lexical_profile.t -> source:Span.source -> (Statement.line list, Diagnostic.t list) Err.t

val parse_expression : Token.slice -> (Rivet_asm_core.Expr.t, error_kind) Err.t
