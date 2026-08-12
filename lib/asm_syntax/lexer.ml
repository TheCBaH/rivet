(* The dialect-aware lexer (.ai/asm_plan.md §4.6).

   Hand-written rather than generated, because what varies between the four
   dialects is not the grammar but the *character classes*: which byte starts a
   comment, which prefixes an immediate, which prefixes a register. A generated
   lexer would need four copies of the same rules with three characters moved,
   and the copies would drift.

   Newlines are significant - an assembly statement ends at one - so [Eol] is a
   token rather than whitespace. *)

open Foundation

(* The lexer's error domain (asm/docs/errors.md). The span travels beside the
   tag rather than inside it because these are *accumulated* and turned into
   diagnostics later, so the failure site is not the site that knows the origin -
   which is the one case §1's "do not duplicate the location" rule exempts.

   [`Bad_number] wraps rather than flattens: the literal is context this layer
   has and [Bigint] does not, and the cause keeps its own typed payload instead
   of being rendered into a sentence (§2). The rendering is unchanged - the
   colon-joined form the lexer built by hand - so the cram baselines still
   match. *)
type error_kind =
  [ `Bad_number of bad_number
  | `Unterminated_string
  | `Unknown_escape of char
  | `Unexpected_character of char ]

and bad_number = { literal : string; reason : Bigint.error }

type error = { kind : error_kind; span : Span.t }

let pp_kind ppf : error_kind -> unit = function
  | `Bad_number { literal; reason } -> Fmt.pf ppf "%s: %a" literal Bigint.pp_error reason
  | `Unterminated_string -> Fmt.string ppf "unterminated string"
  | `Unknown_escape c -> Fmt.pf ppf "unknown escape \\%c" c
  | `Unexpected_character c -> Fmt.pf ppf "unexpected character %C" c

(* One code for the whole domain: a lexical failure is a lexical failure, and
   the four kinds are told apart by the payload rather than by the taxonomy. *)
let kind_code : error_kind -> string = function
  | `Bad_number _ | `Unterminated_string | `Unknown_escape _ | `Unexpected_character _ -> "lex"

let pp_error ppf (e : error) = pp_kind ppf e.kind
let error_code (e : error) = kind_code e.kind
let err kind span = { kind; span }
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start (p : Lexical_profile.t) c = is_alpha c || List.mem c p.identifier_extra
let is_ident_char (p : Lexical_profile.t) c = is_ident_start p c || is_digit c

(* The scanning position is a plain value threaded through arguments and
   return values rather than a mutable field on [cursor]. A scanner that
   fails to recognize what it was trying for - [try_local_label] on an
   ordinary number, [scan_number] on a bad literal - then just returns
   without having touched anything, instead of having to mutate forward and
   remember to undo it. *)
type cursor = { src : Span.source; text : string; profile : Lexical_profile.t }

let span_of c ~start ~stop = Span.of_offset ~source:c.src ~offset:start ~length:(stop - start)
let emit c k ~start ~stop = Token.make k (span_of c ~start ~stop)
let peek c pos off = if pos + off < String.length c.text then Some c.text.[pos + off] else None

let starts_with c pos s =
  let n = String.length s in
  pos + n <= String.length c.text && String.sub c.text pos n = s

(* {1 Numbers}

   GAS integer spellings, which [Bigint.of_string] already parses: a leading [0x]
   or [0b], a leading [0] for octal, otherwise decimal. Scanning stops at the
   first character that cannot belong to the literal, so [0x10(%esp)] lexes as an
   integer followed by a parenthesis rather than as a malformed number. *)
let scan_number c pos =
  let two_char_base =
    starts_with c pos "0x" || starts_with c pos "0X" || starts_with c pos "0b"
    || starts_with c pos "0B"
  in
  let digits_start = if two_char_base then pos + 2 else pos in
  let ok ch = if two_char_base then is_hex ch else is_digit ch in
  let rec scan p = match peek c p 0 with Some ch when ok ch -> scan (p + 1) | _ -> p in
  let stop = scan digits_start in
  let literal = String.sub c.text pos (stop - pos) in
  let span = span_of c ~start:pos ~stop in
  match Bigint.of_string literal with
  | Ok v -> Ok (Token.make (Token.Int v) span, stop)
  | Error e -> Error (err (`Bad_number { literal; reason = Err.Error.kind e }) span)

(* A digit immediately followed by [b] or [f] is a numeric local-label
   reference, not a number followed by an identifier: [1b] is "the most
   recent label 1". Distinguishing them needs one character of lookahead past
   the suffix, because [1b] is a reference and [1bad] is not.

   The digit is capped at exactly one: GAS local labels are a single decimal
   digit, and every definition in this corpus's fixtures and runtime helpers
   uses one. Capping here, rather than scanning a whole digit run and parsing
   it afterwards, means a plain immediate that happens to be glued to a
   trailing [b]/[f] - e.g. a wide x86-64 constant - is never handed to an
   integer parser at all: it falls straight through to [scan_number], whose
   [Bigint] parsing has no width limit, instead of overflowing a parser sized
   to one digit. *)
let try_local_label c pos =
  match peek c pos 0 with
  | Some d1 when is_digit d1 -> (
      match (peek c pos 1, peek c pos 2) with
      | Some (('b' | 'f') as d), next
        when match next with Some ch -> not (is_ident_char c.profile ch) | None -> true ->
          let n = Char.code d1 - Char.code '0' in
          let stop = pos + 2 in
          Some
            ( Token.make
                (Token.Local_label (n, if d = 'b' then `Back else `Forward))
                (span_of c ~start:pos ~stop),
              stop )
      | _ -> None)
  | _ -> None

(* {1 Strings}

   Only the escapes the M1 fixtures and ordinary GAS data directives need. An
   unknown escape is a diagnostic rather than a silent pass-through, so a file
   using one is rejected instead of assembled differently from GAS. *)
let scan_string c pos =
  let rec go p buf =
    match peek c p 0 with
    | None -> Error (err `Unterminated_string (span_of c ~start:pos ~stop:p))
    | Some '"' ->
        Ok
          ( Token.make (Token.String (Buffer.contents buf)) (span_of c ~start:pos ~stop:(p + 1)),
            p + 1 )
    | Some '\\' -> (
        match peek c p 1 with
        | Some 'n' ->
            Buffer.add_char buf '\n';
            go (p + 2) buf
        | Some 't' ->
            Buffer.add_char buf '\t';
            go (p + 2) buf
        | Some 'r' ->
            Buffer.add_char buf '\r';
            go (p + 2) buf
        | Some '0' ->
            Buffer.add_char buf '\000';
            go (p + 2) buf
        | Some '\\' ->
            Buffer.add_char buf '\\';
            go (p + 2) buf
        | Some '"' ->
            Buffer.add_char buf '"';
            go (p + 2) buf
        | Some ch -> Error (err (`Unknown_escape ch) (span_of c ~start:pos ~stop:(p + 1)))
        | None -> Error (err `Unterminated_string (span_of c ~start:pos ~stop:(p + 1))))
    | Some ch ->
        Buffer.add_char buf ch;
        go (p + 1) buf
  in
  go (pos + 1) (Buffer.create 16)

let scan_ident c pos =
  let rec go p =
    match peek c p 0 with Some ch when is_ident_char c.profile ch -> go (p + 1) | _ -> p
  in
  let stop = go pos in
  (String.sub c.text pos (stop - pos), stop)

(* {1 The main loop} *)

let tokenize ~profile ~source =
  let c = { src = source; text = Span.contents source; profile } in
  let rec loop pos acc =
    match peek c pos 0 with
    | None -> Ok (List.rev (emit c Token.Eof ~start:pos ~stop:pos :: acc))
    | Some ch -> (
        let start = pos in
        let one k = (emit c k ~start ~stop:(pos + 1), pos + 1) in
        let two k = (emit c k ~start ~stop:(pos + 2), pos + 2) in
        let continue (tok, stop) = loop stop (tok :: acc) in
        (* Comments run to end of line; the newline itself is still a token,
           because it terminates the statement the comment was attached to. *)
        if List.exists (starts_with c pos) profile.Lexical_profile.comment_introducers then
          let rec skip p = match peek c p 0 with Some '\n' | None -> p | Some _ -> skip (p + 1) in
          loop (skip pos) acc
        else if ch = '\n' then continue (one Token.Eol)
        else if ch = ' ' || ch = '\t' || ch = '\r' then loop (pos + 1) acc
        else if ch = '\\' && peek c pos 1 = Some '\n' then
          (* A line continuation is whitespace, not a statement end. *)
          loop (pos + 2) acc
        else if Some ch = profile.Lexical_profile.statement_separator then continue (one Token.Semi)
        else if Some ch = profile.Lexical_profile.immediate_sigil then
          continue (one Token.Immediate_sigil)
        else if Some ch = profile.Lexical_profile.register_sigil then
          let name, stop = scan_ident c (pos + 1) in
          (* [%] with no name after it is the modulo operator; the dialects
             that use it as a register sigil do not also use it as one, but
             [.section ...,%progbits] shows the sigil in front of something
             that is not a register at all - so this is lexical only, and what
             the name means is the target's business. *)
          let tok = if name = "" then Token.Percent else Token.Register name in
          continue (emit c tok ~start ~stop, stop)
        else if is_digit ch then
          match try_local_label c pos with
          | Some (t, stop) -> loop stop (t :: acc)
          | None -> (
              match scan_number c pos with
              | Ok (t, stop) -> loop stop (t :: acc)
              | Error e -> Error e)
        else if ch = '"' then
          match scan_string c pos with Ok (t, stop) -> loop stop (t :: acc) | Error e -> Error e
        else if
          ch = '.' && match peek c pos 1 with Some c2 -> is_ident_char profile c2 | None -> false
        then
          (* A directive name, or a symbol that happens to start with a dot.
             The parser decides which by position: leading a statement it is a
             directive, inside an operand it is a name. *)
          let name, stop = scan_ident c pos in
          continue (emit c (Token.Directive name) ~start ~stop, stop)
        else if is_ident_start profile ch then
          let name, stop = scan_ident c pos in
          let tok = if name = "." then Token.Dot else Token.Ident name in
          continue (emit c tok ~start ~stop, stop)
        else
          (* A declared modifier, [:lower16:] or [:lo12:], is one token. It is
             matched against the dialect's list rather than recognized as
             "colon, identifier, colon", because that shape is also two labels
             on one line - [foo: bar: mov r0, r1], which GAS accepts. Matching
             a declared name means the dialect decides, and a dialect that
             declares none keeps [:] meaning exactly what it meant before. *)
          let modifier =
            if ch <> ':' then None
            else
              List.find_opt
                (fun m -> starts_with c pos (":" ^ m ^ ":"))
                profile.Lexical_profile.modifier_syntax
          in
          match modifier with
          | Some m ->
              let stop = pos + String.length m + 2 in
              continue (emit c (Token.Modifier m) ~start ~stop, stop)
          | None -> (
              if starts_with c pos "<<" then continue (two Token.Lshift)
              else if starts_with c pos ">>" then continue (two Token.Rshift)
              else
                match ch with
                | ':' -> continue (one Token.Colon)
                | ',' -> continue (one Token.Comma)
                | '(' -> continue (one Token.Lparen)
                | ')' -> continue (one Token.Rparen)
                | '[' -> continue (one Token.Lbracket)
                | ']' -> continue (one Token.Rbracket)
                | '{' -> continue (one Token.Lbrace)
                | '}' -> continue (one Token.Rbrace)
                | '+' -> continue (one Token.Plus)
                | '-' -> continue (one Token.Minus)
                | '*' -> continue (one Token.Star)
                | '/' -> continue (one Token.Slash)
                | '%' -> continue (one Token.Percent)
                | '&' -> continue (one Token.Amp)
                | '|' -> continue (one Token.Pipe)
                | '^' -> continue (one Token.Caret)
                | '~' -> continue (one Token.Tilde)
                | '!' -> continue (one Token.Bang)
                | '=' -> continue (one Token.Equals)
                | '@' -> continue (one Token.At)
                | '.' -> continue (one Token.Dot)
                | ch -> Error (err (`Unexpected_character ch) (span_of c ~start ~stop:(pos + 1)))))
  in
  loop 0 []

let diagnostic_of_error (e : error) =
  Diagnostic.of_error ~origin:(Origin.text e.span) ~code:error_code ~pp:pp_error e
