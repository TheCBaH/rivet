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

let parse_program ~profile ~source =
  match Lexer.tokenize ~profile ~source with
  | Error e -> Error [ Lexer.diagnostic_of_error e ]
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
      try Ok (Grammar.program supply (dummy_lexbuf ()))
      with Grammar.Error ->
        let span =
          match error_span !remaining with
          | Some s -> s
          | None -> Span.of_offset ~source ~offset:0 ~length:0
        in
        Error
          [
            Diagnostic.error ~code:"parse" ~message:"unexpected token" ~origin:(Origin.text span) ();
          ])

(* Parsing a slice as an expression. This is what a directive handler or a
   target operand parser calls; the slice already exists, so the lexer does not
   run again and the spans are the original ones. *)
let parse_expression (slice : Token.slice) =
  match slice with
  | [] -> Error "empty expression"
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
      with Grammar.Error -> Error "malformed expression")

let _ = supplier
