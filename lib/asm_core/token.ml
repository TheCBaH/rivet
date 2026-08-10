(* Lexer tokens.

   A private frontend intermediate (§4.2): the public result of parsing is the
   source AST, and nothing outside the frontend ever sees one of these - except
   as an opaque *slice*, which is exactly the point of §4.7. The common parser
   does not attempt to understand every target operand, so an operand arrives at
   the target's parser as the tokens that spell it, not as a guess at their
   structure. There is no universal grammar covering x86 addressing, ARM
   register lists and AArch64 shift/extend operands at once, and pretending
   otherwise is how a "generic" assembler acquires four special cases. *)

open Foundation

type kind =
  | Ident of string
  | Directive of string  (** a name beginning with [.]; [.text], [.cfi_startproc] *)
  | Int of Bigint.t
  | String of string
  | Register of string  (** only where the dialect has a register sigil: x86's [%eax] *)
  | Local_label of int * [ `Back | `Forward ]  (** [1b], [1f] *)
  | Modifier of string
      (** A relocation modifier the dialect declared: ARM's [:lower16:], AArch64's [:lo12:]. One
          token rather than colon-ident-colon, because [:] otherwise ends a label. *)
  | Colon
  | Comma
  | Semi
  | Immediate_sigil  (** [$] on x86, [#] on ARM and AArch64 *)
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Amp
  | Pipe
  | Caret
  | Tilde
  | Bang
  | Equals
  | Lshift
  | Rshift
  | Dot  (** the current-location operator *)
  | At
  | Eol
  | Eof

type t = { kind : kind; span : Span.t }

let make kind span = { kind; span }
let kind t = t.kind
let span t = t.span

let pp_kind ppf = function
  | Ident s -> Fmt.pf ppf "ident(%s)" s
  | Directive s -> Fmt.pf ppf "directive(%s)" s
  | Int v -> Fmt.pf ppf "int(%a)" Bigint.pp v
  | String s -> Fmt.pf ppf "string(%S)" s
  | Register s -> Fmt.pf ppf "reg(%s)" s
  | Local_label (n, d) -> Fmt.pf ppf "local(%d%s)" n (match d with `Back -> "b" | `Forward -> "f")
  | Modifier m -> Fmt.pf ppf "modifier(%s)" m
  | Colon -> Fmt.string ppf ":"
  | Comma -> Fmt.string ppf ","
  | Semi -> Fmt.string ppf ";"
  | Immediate_sigil -> Fmt.string ppf "imm-sigil"
  | Lparen -> Fmt.string ppf "("
  | Rparen -> Fmt.string ppf ")"
  | Lbracket -> Fmt.string ppf "["
  | Rbracket -> Fmt.string ppf "]"
  | Lbrace -> Fmt.string ppf "{"
  | Rbrace -> Fmt.string ppf "}"
  | Plus -> Fmt.string ppf "+"
  | Minus -> Fmt.string ppf "-"
  | Star -> Fmt.string ppf "*"
  | Slash -> Fmt.string ppf "/"
  | Percent -> Fmt.string ppf "%"
  | Amp -> Fmt.string ppf "&"
  | Pipe -> Fmt.string ppf "|"
  | Caret -> Fmt.string ppf "^"
  | Tilde -> Fmt.string ppf "~"
  | Bang -> Fmt.string ppf "!"
  | Equals -> Fmt.string ppf "="
  | Lshift -> Fmt.string ppf "<<"
  | Rshift -> Fmt.string ppf ">>"
  | Dot -> Fmt.string ppf "."
  | At -> Fmt.string ppf "@"
  | Eol -> Fmt.string ppf "eol"
  | Eof -> Fmt.string ppf "eof"

let pp ppf t = pp_kind ppf t.kind

(* The lowercase tag [--dump-tokens] prints (asm/docs/contracts.md §1.1). Not
   [pp_kind]: that renders the payload too, which is what a debug dump wants and
   what a byte-compared artifact must not have twice - the spelling is already
   the last column. *)
let kind_tag = function
  | Ident _ -> "ident"
  | Directive _ -> "directive"
  | Int _ -> "int"
  | String _ -> "string"
  | Register _ -> "register"
  | Local_label _ -> "local-label"
  | Modifier _ -> "modifier"
  | Colon -> "colon"
  | Comma -> "comma"
  | Semi -> "semi"
  | Immediate_sigil -> "immsigil"
  | Lparen -> "lparen"
  | Rparen -> "rparen"
  | Lbracket -> "lbracket"
  | Rbracket -> "rbracket"
  | Lbrace -> "lbrace"
  | Rbrace -> "rbrace"
  | Plus -> "plus"
  | Minus -> "minus"
  | Star -> "star"
  | Slash -> "slash"
  | Percent -> "percent"
  | Amp -> "amp"
  | Pipe -> "pipe"
  | Caret -> "caret"
  | Tilde -> "tilde"
  | Bang -> "bang"
  | Equals -> "equals"
  | Lshift -> "lshift"
  | Rshift -> "rshift"
  | Dot -> "dot"
  | At -> "at"
  | Eol -> "eol"
  | Eof -> "eof"

(* {1 Slices}

   A run of tokens, which is how an operand and a directive argument reach the
   code that understands them. *)

type slice = t list

let slice_span = function
  | [] -> None
  | first :: rest ->
      let last = List.fold_left (fun _ t -> t) first rest in
      Some
        (Span.make ~source:(Span.source_of first.span) ~offset:(Span.offset first.span)
           ~length:(Span.offset last.span + Span.length last.span - Span.offset first.span)
           ~line:(Span.line first.span) ~column:(Span.column first.span))

(* The spelling of a slice, with a space wherever the source had whitespace
   between two tokens. [.arch armv7-a] must come back as ["armv7-a"] and
   [.arch armv7 - a] as ["armv7 - a"] - a handler that reconstructed the text
   from token kinds alone could not tell those apart, and GAS treats them
   differently. *)
let slice_text (ts : slice) =
  let buf = Buffer.create 32 in
  let rec go prev_end = function
    | [] -> ()
    | t :: rest ->
        (match prev_end with
        | Some e when e < Span.offset t.span -> Buffer.add_char buf ' '
        | _ -> ());
        Buffer.add_string buf (Span.text t.span);
        go (Some (Span.offset t.span + Span.length t.span)) rest
  in
  go None ts;
  Buffer.contents buf

let pp_slice ppf ts = Fmt.pf ppf "@[<h>%a@]" Fmt.(list ~sep:(any " ") pp) ts
