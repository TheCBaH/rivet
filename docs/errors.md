# The error model

The reference for anyone adding a failure case. §4 records how the tree got
here, because the shape is reusable.

Two things are called errors here and they are not the same thing:

- A **typed error** is a value in a module's own polymorphic-variant error
  domain. It carries the data that describes what went wrong. This is the
  currency: it is what a function returns, what crosses a phase boundary, and
  what a caller pattern-matches.
- A **`Diagnostic.t`** is the *presentation* of one — severity, code, message,
  origin, notes — built once, at the reporting boundary, by
  `Diagnostic.of_error`. It is what a user reads and what the cram baselines
  pin. Nothing internal consumes it.

Wrapping both is `Err`, the vendored [err_trace](../vendor/err_trace/README.md)
library: `('a, 'e) Err.t = ('a, 'e Err.Error.t) result` keeps the typed payload
while attaching a bounded detection origin and a bounded semantic event trail.

Note the division of labour between the two origins, which is the reason both
exist. `Foundation.Origin` says where in the **assembly source** a problem is —
a span, a producer, a synthesizing pass. `Err.Error.origin` says where in the
**assembler** it was detected, as an ordinary `__POS__`. Neither substitutes for
the other, and a diagnostic that misattributes the first is diagnosed with the
second.

---

## 1. What a variant constructor carries

Five rules. Each exists because the tree used to break it.

**Carry the domain value, not its rendering.** `Token.slice`, not
`Token.slice_text slice`. `Expr.t`, not `Expr.to_string e`. `Opcode.t`, not
`Opcode.name op`. `Bigint.t`, not its decimal spelling. The value is already in
hand at the failure site; rendering it there is precisely what destroys it. The
domain's `pp` re-derives the text later, from the value, at the one place that
is allowed to produce text.

**Carry the cause when there was one.** Never `| Error _ ->`. If a call failed
and its error is being replaced by one of ours, the original belongs in a field.
The site this rule is named after read:

```ocaml
match Parse_lines.parse_expression slice with
| Ok e    -> Ok (Operand.Sym e)
| Error _ -> bad ("cannot parse operand " ^ Token.slice_text slice)
```

which loses the cause, reduces the input to a re-rendering of itself, and
returns a sentence. It is now:

```ocaml
`Cannot_parse_operand of { slice : Token.slice; reason : Parse_lines.error }
```

**Carry what the caller could not know.** `no %s form takes these operands`
names the mnemonic and not the operands, so the reader is sent back to the
source line to find out which operands those were. The operand list is the part
that makes the message actionable, so it is in the constructor.

**Do not duplicate the location.** Constructors do not carry `Origin.t`. The
failure site already passes `~origin` to `Diagnostic.of_error`, derived from the
value (`slice_origin slice`) or from the enclosing pass. Two locations that can
disagree are worse than one. The exception is `Lexer.error` and
`Local_labels.error`, which are *accumulated* and converted later, so they keep
an explicit `span` field.

**Keep payloads proportionate.** A slice, an expression, an opcode, a segment
name — values already alive in the caller. Never a whole AST module or an image.
`Err` retains the payload by design, unlike its `Map` *events*, which
deliberately drop the previous payload so that a long conversion chain cannot
pin large values alive.

### Domain types are closed rows

A domain is built by inclusion but is itself closed:

```ocaml
type t = [ Target_error.t | `Unknown_register of string | ... ]

let pp ppf = function
  | #Target_error.t as e -> Target_error.pp ppf e
  | `Unknown_register n  -> Fmt.pf ppf "unknown register %%%s" n
  | ...
```

so `pp` and `code` are exhaustiveness-checked by the compiler. That is the
property strings never had, and it is what stops a new constructor being added
without deciding how it renders and which code it reports under.

One syntactic wrinkle: OCaml rejects `[ Base.t ]` — a row that includes a type
and nothing else is a parse error. So a domain that has not yet grown a real tag
reads as a plain alias and becomes a row with its first constructor. During the
migration this was the common case; it is now rare, but the rule still bites
when a domain is created before its second tag.

### Where the location lives

Three domains carry their own location rather than leaving it to the failure
site, and they are the documented exception to "do not duplicate the location":
`Lexer.error` and `Local_labels.error` keep a `span`, and `Target_error.t` keeps
an `Origin.t`. All three are *accumulated* — the site that builds the error is
not the site that reports it. `Target_error` is the sharpest case:
`parse_one_operand` knows the slice its operand came from, while the pipeline
that renders the failure knows only the statement.

---

## 2. Moving between error domains

Four mechanisms. Three of them already existed in the tree implicitly; the
policy is to make the choice explicit, and to forbid the fourth.

| | When | How |
|---|---|---|
| **Share** | The tag means the same thing in both domains *and reports under the same code*. Nothing is converted. | Row inclusion; `pp` delegates with `\| #Base.t as e -> Base.pp ppf e`. No `Err` call, no event — there is no conversion to record. |
| **Wrap** | The source error is a *cause*, and the destination adds context the source could not know. | A named field on a destination tag: `` `Fixup of { name : string; reason : Expr.error } ``, whose `pp` renders `"fixup %s: %a"`. The destination owns the code. |
| **Delegate the code** | The source layer is the only one that could have detected the mistake, so it is the one that can name it. | The source exports `code : t -> string option`; the wrapper uses `Option.value (Codec.code e) ~default:own_code`. `Codec` is the case: a target knows its own mnemonics and can reject a bad suffix with a source span, but a direct-lowered producer hands the codec a rung name out of nowhere and nothing above has the ladder to check it against. |
| **Flatten to a string** | Never. | — |

And the rules that go with them:

- Every crossing that changes the payload **type** goes through
  `Err.map_error ~pos:__POS__ ~pp_error:Dest.pp` — or `Err.Error.map_kind
  ~pos:__POS__` when the code already holds a wrapper. That call is what records
  the `Map` event at the position of the conversion, which is how the trail
  comes to spell out `parse → simplify → lower → bind` instead of it being a
  convention in the diagnostic code prefix. Sharing needs no call: nothing was
  converted.
- The **destination** supplies `~pp_error`, never the source. `Err`'s `Map`
  event retains neither the previous payload nor either printer, so the printer
  passed at a boundary describes the domain that exists *after* it.
- A domain never re-codes an error it merely passes through, and always
  re-codes one it wraps, unless the source delegated.
- `Diagnostic.of_error` is called **once**, at the reporting boundary, against
  the top-level union. Intermediate layers move variants, not diagnostics.

`Foundation.Diag` names the combinations that recur, so no caller has to spell
out `Err.fail ~pp_error:Diagnostic.pp_all` and risk getting the printer wrong.

### Accumulating independent failures

Independent checks that already run to completion use `Err.Accum`, normally
through `Diag.validate_all` once they have become diagnostics. Each failure is
then detected with its own `Err.Error.t`, origin, and monitor observation before
the batch is collapsed into the stage's existing `Diagnostic.t list` payload.
The collapsed wrapper retains the first failure's provenance, while diagnostic
order remains input order.

This is not a mechanical replacement for the mutable collectors in stateful
passes. Simplification, lowering, and layout build ordered output while carrying
state from one item to the next; converting those loops to an applicative walk
would obscure their sequencing without making the checks independent. As with
any accumulating traversal over untrusted input, establish the input-size or
cost bound before entering `Err.Accum`.

---

## 3. Determinism

`Err` has two independent nondeterminism sources. `Foundation.Err_policy`
disables one and keeps the other, and the distinction matters:

- **Automatic stack capture must be off.** `Err.Config.default` — what `Err`
  uses if nobody says otherwise — captures a `Printexc` callstack at every
  detection. That text carries host paths and frame counts, which §3.7 bans from
  any diagnostic string, and it differs across the three backends §11.6
  requires. `Err_policy.deterministic` sets `backtrace:Never`.
- **Explicit positions and the event trail stay.** `__POS__` is a compile-time
  constant and an event under `Never` carries nothing else, so both are
  deterministic. This is why the policy is spelled out rather than taken from
  `Err.Config.fast`: `fast` is deterministic but selects no actions at all, so
  it would discard the trail along with the stacks.

Independently of all that:

> **No `Err` origin, stack, event, or observation text may be rendered into an
> expect or cram baseline.**

Determinism makes this rule cheap to hold; it does not make it unnecessary.
`Diagnostic.pp` and `Diagnostic.pp_repr` render the diagnostic only. The CLI's
tracing switch writes to stderr for the same reason.

---

### Asking for it

The provenance is off by default and never reaches stdout. `asm --err_trace
boundaries` installs a monitor that writes to stderr:

```
[err] t.s:3:2: error[x86.simplify]: unknown instruction bogusinsn
        bogusinsn %eax
        ^^^^^^^^^
      detected at:
        driver/pipeline.ml:209:41-48
      trace:
        mapped at driver/pipeline.ml:506:34-41
```

Two locations, and they answer different questions. `t.s:3:2` is
`Foundation.Origin` — where in the assembly source. `driver/pipeline.ml:209` is
`Err.Error.origin` — where in the assembler it was detected. The `trace:` line
is the phase crossing recorded by `Diag.stage`.

The switch pins `backtrace` to `off` rather than passing the spec through:
what it is for is the `__POS__` trail, which is a compile-time constant and
reads the same on every backend, where a captured native stack would add host
paths and frame counts that differ between native, js_of_ocaml and Melange.

## 4. How the migration ran

Replacing ~160 string-built diagnostics is two changes, and only one of them can
move committed output — 63 cram lines pin `origin: error[code]: message`. They
are separated so the dangerous lines are not hidden among the mechanical ones.

`Foundation.Legacy_error` was the seam:

```ocaml
type t = [ `Diag of { code : string; message : string } ]
```

Every domain is born as `type t = [ Legacy_error.t ]` and every existing site is
retyped to carry its code and its already-built message verbatim. That pass
delivers the whole architecture — domains, `Err` wrappers, `~pos:__POS__`
origins, the union, `of_error`, the crossings — and **cannot** change a rendered
character, because both strings travel through untouched.

Each later commit emptied one module: real constructors in, `` `Diag `` uses
out, and when the last one went the tag was dropped from that domain's row.

`tools/asm-check-errors.sh` ratcheted that, one commit at a time: per-file
`Legacy_error` counts that could fall but never rise, and no file outside the
baseline could acquire the row. It rose exactly once, when splitting the target
domain in two created three new front-end domains - a reviewed regeneration, not
a regression. When the baseline emptied, it and `Legacy_error` were deleted
together, which is why neither is in the tree.

### Erasure boundaries

Two places cannot carry a target's error type, and both are named rather than
implicit. `Image` stores a target-supplied fixup evaluator inside a `laid_out`,
and `Target.DRIVER` erases the architecture entirely — so at
`ENCODE.error_diagnostic` a target renders its own domain and a `Diagnostic.t`
is what crosses. Below those calls the payload survives intact; the erasure is
where a typed error legitimately becomes text before the reporting boundary, and
it is the reason `DRIVER` speaks in `Diag.t` while everything under it speaks in
its own domain.

The end state is enforced by `diagnostic.mli`. The `make` / `error` / `warning`
constructors that took `~code:string ~message:string` are gone, so `of_error` is
the only way to build a diagnostic and a hand-written message is not
expressible - there is no function left that accepts one.
