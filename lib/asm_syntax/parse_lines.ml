(* Driving the generated parser.

   Menhir's classic entry points want a [Lexing.lexbuf] and a token function.
   The lexbuf here is a placeholder: positions come from the [Token.t] each
   terminal carries, so nothing reads them. Doing it this way rather than
   threading [lex_start_p]/[lex_curr_p] through the adapter means there is one
   notion of position in the frontend - [Span.t] - instead of two that must be
   kept in agreement. *)

open Foundation

let to_menhir (t : Token.t) : Grammar.token =
  match Token.kind t with
  | Token.Ident _ -> Grammar.IDENT t
  | Token.Directive _ -> Grammar.DIRECTIVE t
  | Token.Int _ -> Grammar.INT t
  | Token.String _ -> Grammar.STRING t
  | Token.Register _ -> Grammar.REGISTER t
  | Token.Local_label _ -> Grammar.LOCAL t
  | Token.Modifier _ -> Grammar.MODIFIER t
  | Token.Colon -> Grammar.COLON t
  | Token.Comma -> Grammar.COMMA t
  | Token.Semi -> Grammar.SEMI t
  | Token.Immediate_sigil -> Grammar.IMMSIGIL t
  | Token.Lparen -> Grammar.LPAREN t
  | Token.Rparen -> Grammar.RPAREN t
  | Token.Lbracket -> Grammar.LBRACKET t
  | Token.Rbracket -> Grammar.RBRACKET t
  | Token.Lbrace -> Grammar.LBRACE t
  | Token.Rbrace -> Grammar.RBRACE t
  | Token.Plus -> Grammar.PLUS t
  | Token.Minus -> Grammar.MINUS t
  | Token.Star -> Grammar.STAR t
  | Token.Slash -> Grammar.SLASH t
  | Token.Percent -> Grammar.PERCENT t
  | Token.Amp -> Grammar.AMP t
  | Token.Pipe -> Grammar.PIPE t
  | Token.Caret -> Grammar.CARET t
  | Token.Tilde -> Grammar.TILDE t
  | Token.Bang -> Grammar.BANG t
  | Token.Equals -> Grammar.EQUALS t
  | Token.Lshift -> Grammar.LSHIFT t
  | Token.Rshift -> Grammar.RSHIFT t
  | Token.Dot -> Grammar.DOT t
  | Token.At -> Grammar.AT t
  | Token.Eol -> Grammar.EOL t
  | Token.Eof -> Grammar.EOF t

(* The grammar requires every line to be terminated, so a file whose last line
   has no newline gets one here rather than in the grammar. Making it the
   grammar's problem would mean an optional terminator, which is one more state
   in which a missing separator is silently accepted. *)
let ensure_terminated tokens =
  let rec go = function
    | [ ({ Token.kind = Token.Eof; span } as eof) ] -> [ Token.make Token.Eol span; eof ]
    | ({ Token.kind = Token.Eol; _ } as e) :: [ ({ Token.kind = Token.Eof; _ } as eof) ] ->
        [ e; eof ]
    | t :: rest -> t :: go rest
    | [] -> []
  in
  go tokens

let supplier tokens =
  let remaining = ref tokens in
  fun _lexbuf ->
    match !remaining with
    | [] ->
        Grammar.EOF
          (Token.make Token.Eof
             (Span.of_offset ~source:(Span.source ~name:"" ~contents:"") ~offset:0 ~length:0))
    | t :: rest ->
        remaining := rest;
        to_menhir t

let error_span tokens =
  (* Menhir's classic API reports the error through an exception with no
     payload, so the location is recovered as "the token the supplier had not
     yet consumed". That is the token the parser choked on. *)
  match tokens with
  | t :: _ -> Some (Token.span t)
  | [] -> None

let dummy_lexbuf () = Lexing.from_string ""

(* This module's error domain. One tag, because Menhir's classic API reports
   every syntax failure the same way; what varies is the token it stopped on,
   which is now carried rather than discarded. *)
type error_kind =
  [ `Unexpected_token of Token.t option | `Empty_expression | `Malformed_expression of Token.slice ]

let pp_error ppf : error_kind -> unit = function
  | `Unexpected_token _ -> Fmt.string ppf "unexpected token"
  | `Empty_expression -> Fmt.string ppf "empty expression"
  | `Malformed_expression _ -> Fmt.string ppf "malformed expression"

let error_code : error_kind -> string = function
  | `Unexpected_token _ -> "parse"
  | `Empty_expression | `Malformed_expression _ -> "parse.expression"

let parse_diagnostic ~span kind =
  Diagnostic.of_error ~origin:(Origin.text span) ~code:error_code ~pp:pp_error kind

let parse_program ~profile ~source =
  match Lexer.tokenize ~profile ~source with
  (* Shared: a lexical failure is reported here exactly as the lexer described
     it, under the lexer's own code (asm/docs/errors.md §2). *)
  | Error e -> Diag.fail ~pos:__POS__ [ Lexer.diagnostic_of_error e ]
  | Ok tokens -> (
      let tokens = ensure_terminated tokens in
      let remaining = ref tokens in
      let supply _lexbuf =
        match !remaining with
        | [] -> Grammar.EOF (List.nth tokens (List.length tokens - 1))
        | t :: rest ->
            remaining := rest;
            to_menhir t
      in
      (* [Err.protect] rather than a bare [try]: it names the exception this
         boundary is allowed to absorb and re-raises everything else, where the
         previous handler would have caught only [Grammar.Error] but recorded
         nothing about the crossing. The [Catch] event it adds is what says a
         failure entered our domain from a foreign one (asm/docs/errors.md §2).

         The token is the one the supplier had not yet consumed - what the
         parser choked on - and is now carried rather than reduced to its
         span. *)
      let stopped_on = ref None in
      match
        Err.protect ~pos:__POS__ ~pp_error
          ~catch:(fun exn ->
            match exn with
            | Grammar.Error ->
                (stopped_on := match !remaining with t :: _ -> Some t | [] -> None);
                Some (`Unexpected_token !stopped_on)
            | _ -> None)
          (fun () -> Grammar.program supply (dummy_lexbuf ()))
      with
      | Ok lines -> Ok lines
      | Error e ->
          let span =
            match !stopped_on with
            | Some t -> Token.span t
            | None -> Span.of_offset ~source ~offset:0 ~length:0
          in
          Diag.fail ~pos:__POS__ [ parse_diagnostic ~span (Err.Error.kind e) ])

(* Parsing a slice as an expression. This is what a directive handler or a
   target operand parser calls; the slice already exists, so the lexer does not
   run again and the spans are the original ones. *)
let parse_expression (slice : Token.slice) =
  match slice with
  | [] -> Err.fail ~pos:__POS__ ~pp_error `Empty_expression
  | first :: _ -> (
      let eof = Token.make Token.Eof (Token.span first) in
      let remaining = ref (slice @ [ eof ]) in
      let supply _lexbuf =
        match !remaining with
        | [] -> Grammar.EOF eof
        | t :: rest ->
            remaining := rest;
            to_menhir t
      in
      try Ok (Grammar.expression_only supply (dummy_lexbuf ()))
      with Grammar.Error -> Err.fail ~pos:__POS__ ~pp_error (`Malformed_expression slice))

let _ = supplier
