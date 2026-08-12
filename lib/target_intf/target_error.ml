(* What every target's error domain has in common (asm/docs/errors.md).

   Two things, and the second is smaller than it looks.

   {1 The origin pairing}

   §1 says a constructor should not carry [Origin.t], because the failure site
   already passes one to [Diagnostic.of_error]. Targets are the documented
   exception, for the same reason the lexer is: the site that knows the origin is
   not the site that reports it. [parse_one_operand] knows the slice its operand
   came from; the pipeline that renders the failure knows only the statement.
   Fusing the origin into a rendered diagnostic - which is what the code did
   before any of this - keeps the location and destroys the payload, so the pair
   is what preserves both.

   [t] is parameterized over the kind rather than fixing one. Each target
   instantiates it twice: once for its encoder's failures and once for its front
   end's, which are different domains for the reason {!Target.TARGET} explains.

   {1 The shared row}

   Deliberately four tags. The obvious candidates mostly are not shared: x86
   decodes "no form matches these bytes" where the two fixed-width targets match
   a *word*, and every range, alignment and padding message names something
   architecture-specific. Forcing those into one row would mean carrying a
   discriminator whose only job is to pick the wording back apart - a worse
   version of the string it replaced.

   What is left is genuinely common. A target includes it as
   [ Target_error.shared | ... ] and delegates in its printer with
   [ #Target_error.shared as e ].

   Codes are deliberately not here. The same tag is [x86.simplify] on one target
   and [arm.lower] on another, because the phase that detects it differs, so the
   target names it.

   Nothing here mentions an architecture: tools/asm-check-layers.sh rejects any
   source under lib/ that does, and a tag describing a *kind* of failure never
   needs to. *)

open Foundation

type 'kind t = { kind : 'kind; origin : Origin.t }

let make ~origin kind = { kind; origin }
let kind e = e.kind
let origin e = e.origin

(* The erasure boundary, named so it is visible as one. {!Image} holds a
   target-supplied fixup evaluator and {!Target.DRIVER} erases the architecture
   entirely, so neither can carry a target's error type; a target renders here
   and a diagnostic is what crosses. Everything below this call keeps the
   payload. *)
let to_diagnostic ~code ~pp e = Diagnostic.of_error ~origin:e.origin ~code ~pp e.kind

type shared =
  [ `Unknown_instruction of string  (** the mnemonic no opcode table claims *)
  | `No_form of string  (** the opcode name; no encoding accepts these operands *)
  | `Immediate_too_wide  (** an immediate that does not fit int64 *)
  | `Decode_no_normalized  (** a form decoded, and produced no instruction *) ]

let pp_shared ppf : shared -> unit = function
  | `Unknown_instruction m -> Fmt.pf ppf "unknown instruction %s" m
  | `No_form op -> Fmt.pf ppf "no %s form takes these operands" op
  | `Immediate_too_wide -> Fmt.string ppf "immediate does not fit 64 bits"
  | `Decode_no_normalized -> Fmt.string ppf "decoded a form with no normalized instruction"
