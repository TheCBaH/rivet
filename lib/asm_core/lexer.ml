(* The dialect-aware lexer (.ai/asm_plan.md §4.6).

   Hand-written rather than generated, because what varies between the four
   dialects is not the grammar but the *character classes*: which byte starts a
   comment, which prefixes an immediate, which prefixes a register. A generated
   lexer would need four copies of the same rules with three characters moved,
   and the copies would drift.

   Newlines are significant - an assembly statement ends at one - so [Eol] is a
   token rather than whitespace. *)

open Foundation

type error = { message : string; span : Span.t }

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start (p : Lexical_profile.t) c = is_alpha c || List.mem c p.identifier_extra
let is_ident_char (p : Lexical_profile.t) c = is_ident_start p c || is_digit c

type state = { src : Span.source; text : string; profile : Lexical_profile.t; mutable pos : int }

let span_of st ~start = Span.of_offset ~source:st.src ~offset:start ~length:(st.pos - start)
let peek st off = if st.pos + off < String.length st.text then Some st.text.[st.pos + off] else None

let starts_with st s =
  let n = String.length s in
  st.pos + n <= String.length st.text && String.sub st.text st.pos n = s

(* {1 Numbers}

   GAS integer spellings, which [Bigint.of_string] already parses: a leading [0x]
   or [0b], a leading [0] for octal, otherwise decimal. Scanning stops at the
   first character that cannot belong to the literal, so [0x10(%esp)] lexes as an
   integer followed by a parenthesis rather than as a malformed number. *)
let scan_number st =
  let start = st.pos in
  let two_char_base =
    starts_with st "0x" || starts_with st "0X" || starts_with st "0b" || starts_with st "0B"
  in
  if two_char_base then st.pos <- st.pos + 2;
  let ok c = if two_char_base then is_hex c else is_digit c in
  let rec go () =
    match peek st 0 with
    | Some c when ok c ->
        st.pos <- st.pos + 1;
        go ()
    | _ -> ()
  in
  go ();
  let literal = String.sub st.text start (st.pos - start) in
  match Bigint.of_string literal with
  | Ok v -> Ok (Token.make (Token.Int v) (span_of st ~start))
  | Error m -> Error { message = Printf.sprintf "%s: %s" literal m; span = span_of st ~start }

(* A digit immediately followed by [b] or [f] is a numeric local-label
   reference, not a number followed by an identifier: [1b] is "the most recent
   label 1". Distinguishing them needs one character of lookahead past the
   suffix, because [1b] is a reference and [1bad] is not. *)
let try_local_label st =
  match peek st 0 with
  | Some c when is_digit c -> (
      let start = st.pos in
      let rec digits () =
        match peek st 0 with
        | Some c when is_digit c ->
            st.pos <- st.pos + 1;
            digits ()
        | _ -> ()
      in
      digits ();
      let n = int_of_string (String.sub st.text start (st.pos - start)) in
      match (peek st 0, peek st 1) with
      | Some (('b' | 'f') as d), next
        when match next with Some c -> not (is_ident_char st.profile c) | None -> true ->
          st.pos <- st.pos + 1;
          Some
            (Token.make
               (Token.Local_label (n, if d = 'b' then `Back else `Forward))
               (span_of st ~start))
      | _ ->
          st.pos <- start;
          None)
  | _ -> None

(* {1 Strings}

   Only the escapes the M1 fixtures and ordinary GAS data directives need. An
   unknown escape is a diagnostic rather than a silent pass-through, so a file
   using one is rejected instead of assembled differently from GAS. *)
let scan_string st =
  let start = st.pos in
  st.pos <- st.pos + 1;
  let buf = Buffer.create 16 in
  let rec go () =
    match peek st 0 with
    | None -> Error { message = "unterminated string"; span = span_of st ~start }
    | Some '"' ->
        st.pos <- st.pos + 1;
        Ok (Token.make (Token.String (Buffer.contents buf)) (span_of st ~start))
    | Some '\\' -> (
        st.pos <- st.pos + 1;
        match peek st 0 with
        | Some 'n' ->
            Buffer.add_char buf '\n';
            st.pos <- st.pos + 1;
            go ()
        | Some 't' ->
            Buffer.add_char buf '\t';
            st.pos <- st.pos + 1;
            go ()
        | Some 'r' ->
            Buffer.add_char buf '\r';
            st.pos <- st.pos + 1;
            go ()
        | Some '0' ->
            Buffer.add_char buf '\000';
            st.pos <- st.pos + 1;
            go ()
        | Some '\\' ->
            Buffer.add_char buf '\\';
            st.pos <- st.pos + 1;
            go ()
        | Some '"' ->
            Buffer.add_char buf '"';
            st.pos <- st.pos + 1;
            go ()
        | Some c ->
            Error { message = Printf.sprintf "unknown escape \\%c" c; span = span_of st ~start }
        | None -> Error { message = "unterminated string"; span = span_of st ~start })
    | Some c ->
        Buffer.add_char buf c;
        st.pos <- st.pos + 1;
        go ()
  in
  go ()

let scan_ident st =
  let start = st.pos in
  let rec go () =
    match peek st 0 with
    | Some c when is_ident_char st.profile c ->
        st.pos <- st.pos + 1;
        go ()
    | _ -> ()
  in
  go ();
  (String.sub st.text start (st.pos - start), start)

(* {1 The main loop} *)

let tokenize ~profile ~source =
  let st = { src = source; text = Span.contents source; profile; pos = 0 } in
  let out = ref [] in
  let error = ref None in
  let emit k start = out := Token.make k (span_of st ~start) :: !out in
  let rec loop () =
    if !error <> None then ()
    else
      match peek st 0 with
      | None ->
          let start = st.pos in
          emit Token.Eof start
      | Some c ->
          let start = st.pos in
          (* Comments run to end of line; the newline itself is still a token,
             because it terminates the statement the comment was attached to. *)
          if List.exists (starts_with st) profile.Lexical_profile.comment_introducers then (
            let rec skip () =
              match peek st 0 with
              | Some '\n' | None -> ()
              | Some _ ->
                  st.pos <- st.pos + 1;
                  skip ()
            in
            skip ();
            loop ())
          else if c = '\n' then (
            st.pos <- st.pos + 1;
            emit Token.Eol start;
            loop ())
          else if c = ' ' || c = '\t' || c = '\r' then (
            st.pos <- st.pos + 1;
            loop ())
          else if c = '\\' && peek st 1 = Some '\n' then (
            (* A line continuation is whitespace, not a statement end. *)
            st.pos <- st.pos + 2;
            loop ())
          else if Some c = profile.Lexical_profile.statement_separator then (
            st.pos <- st.pos + 1;
            emit Token.Semi start;
            loop ())
          else if Some c = profile.Lexical_profile.immediate_sigil then (
            st.pos <- st.pos + 1;
            emit Token.Immediate_sigil start;
            loop ())
          else if Some c = profile.Lexical_profile.register_sigil then (
            st.pos <- st.pos + 1;
            let name, _ = scan_ident st in
            (* [%] with no name after it is the modulo operator; the dialects
               that use it as a register sigil do not also use it as one, but
               [.section ...,%progbits] shows the sigil in front of something
               that is not a register at all - so this is lexical only, and what
               the name means is the target's business. *)
            if name = "" then emit Token.Percent start else emit (Token.Register name) start;
            loop ())
          else if is_digit c then
            match try_local_label st with
            | Some t ->
                out := t :: !out;
                loop ()
            | None -> (
                match scan_number st with
                | Ok t ->
                    out := t :: !out;
                    loop ()
                | Error e -> error := Some e)
          else if c = '"' then
            match scan_string st with
            | Ok t ->
                out := t :: !out;
                loop ()
            | Error e -> error := Some e
          else if
            c = '.' && match peek st 1 with Some c2 -> is_ident_char profile c2 | None -> false
          then (
            (* A directive name, or a symbol that happens to start with a dot.
               The parser decides which by position: leading a statement it is a
               directive, inside an operand it is a name. *)
            let name, _ = scan_ident st in
            emit (Token.Directive name) start;
            loop ())
          else if is_ident_start profile c then (
            let name, _ = scan_ident st in
            if name = "." then emit Token.Dot start else emit (Token.Ident name) start;
            loop ())
          else
            let two k =
              st.pos <- st.pos + 2;
              emit k start
            in
            let one k =
              st.pos <- st.pos + 1;
              emit k start
            in
            (* A declared modifier, [:lower16:] or [:lo12:], is one token. It is
               matched against the dialect's list rather than recognized as
               "colon, identifier, colon", because that shape is also two labels
               on one line - [foo: bar: mov r0, r1], which GAS accepts. Matching
               a declared name means the dialect decides, and a dialect that
               declares none keeps [:] meaning exactly what it meant before. *)
            let modifier =
              if c <> ':' then None
              else
                List.find_opt
                  (fun m -> starts_with st (":" ^ m ^ ":"))
                  profile.Lexical_profile.modifier_syntax
            in
            (match modifier with
            | Some m ->
                st.pos <- st.pos + String.length m + 2;
                emit (Token.Modifier m) start
            | None -> (
                if starts_with st "<<" then two Token.Lshift
                else if starts_with st ">>" then two Token.Rshift
                else
                  match c with
                  | ':' -> one Token.Colon
                  | ',' -> one Token.Comma
                  | '(' -> one Token.Lparen
                  | ')' -> one Token.Rparen
                  | '[' -> one Token.Lbracket
                  | ']' -> one Token.Rbracket
                  | '{' -> one Token.Lbrace
                  | '}' -> one Token.Rbrace
                  | '+' -> one Token.Plus
                  | '-' -> one Token.Minus
                  | '*' -> one Token.Star
                  | '/' -> one Token.Slash
                  | '%' -> one Token.Percent
                  | '&' -> one Token.Amp
                  | '|' -> one Token.Pipe
                  | '^' -> one Token.Caret
                  | '~' -> one Token.Tilde
                  | '!' -> one Token.Bang
                  | '=' -> one Token.Equals
                  | '@' -> one Token.At
                  | '.' -> one Token.Dot
                  | c ->
                      st.pos <- st.pos + 1;
                      error :=
                        Some
                          {
                            message = Printf.sprintf "unexpected character %C" c;
                            span = span_of st ~start;
                          }));
            loop ()
  in
  loop ();
  match !error with Some e -> Error e | None -> Ok (List.rev !out)

let diagnostic_of_error (e : error) =
  Diagnostic.error ~code:"lex" ~message:e.message ~origin:(Origin.text e.span) ()
