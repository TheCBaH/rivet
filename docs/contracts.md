# Public OCaml contracts

Frozen at M0.5; §5 added at M2, when fixups stopped being a declared model and
started being constructed.

This document freezes five things: the **canonical dump format** at every
boundary, the **`form_id` scheme**, the **per-dialect directive table**, the
**support matrix**, and the **fixup and relaxation contract**. Everything here is
a contract in the same sense as `exec-abi-v1.md`: a change to any of it is a
versioned change with a migration note, not an edit.

The reason to freeze these and not, say, the module signatures, is that these
five are what the *tests* compare. A dump format that drifts silently turns
every cram test into a rubber stamp; a `form_id` that drifts turns "the same
encoding decision" into an unfalsifiable claim; a directive table that drifts
makes an unknown directive assemble; a support matrix that drifts makes "not
implemented" and "not tested" indistinguishable; and a fixup contract that
drifts makes four targets disagree about what an addend is. Signatures, by
contrast, are checked by the compiler on every build and do not need a document
to hold them.

Scope note: this freezes the *format* of each dump, not its *content*. Adding a
target adds rows. Adding an instruction adds lines. Neither is a contract change.
Changing what a column means, or what a line looks like, is.

---

## 1. Canonical dump format per boundary

Seven boundaries, seven dumps, one flag each. Every dump is **line-oriented, ASCII,
`\n`-terminated, with no trailing whitespace and no locale-dependent
formatting**, because these files are compared byte-for-byte across three
runtimes (native, js_of_ocaml/Node, Melange/Node) and a difference must always
mean a difference in the assembler.

Three rules apply to all seven:

- **Numbers.** Addresses and encodings are lowercase hexadecimal with an `0x`
  prefix and no leading zeroes beyond one digit (`0x0`, `0x40000000`). Sizes,
  offsets, alignments and counts are decimal. Bytes inside a hex run are
  lowercase, two digits, space-separated, no prefix.
- **Ordering.** Every list is printed in a deterministic order that is stated
  per dump below. Where the order is "as written", it is the input order and not
  a sort — a sorted dump would hide a reordering bug in layout.
- **No timestamps, no paths, no versions.** A dump that mentions the wall clock,
  the working directory or the compiler version cannot be compared across runs,
  and the temptation to normalize it away in the harness is how a real difference
  gets normalized away too.

### 1.1 `--dump-tokens` — the lexer

One token per line:

```text
<offset> <length> <kind> <spelling>
```

`offset` and `length` are decimal byte offsets into the source. `kind` is the
lowercase constructor name (`ident`, `directive`, `int`, `string`, `register`,
`local-label`, `colon`, `comma`, `semi`, `immsigil`, `lparen`, …, `eol`, `eof`).
`spelling` is the exact source text the span covers. Two tokens have no source
text that can appear in a column: an `eol` spells itself as a newline, which
would end the dump line it sits on, and an `eof` has no spelling at all. Those
two print as the two-character `\n` and as nothing respectively. Nothing else is
escaped, because nothing else can contain a newline.

This dump exists to make dialect differences visible: the same line lexed under
two profiles is the cheapest possible demonstration that `#` is a comment on x86
and an immediate sigil on ARM.

### 1.2 `--dump-source-ast` — after parsing

```text
source <unit-name>
label <name>
directive <name> [<arg-slice>] [<arg-slice>]…
assign <name> = <expr>
insn <target-rendered-surface-instruction>
```

Items in input order. Directive arguments print as bracketed **token slices**,
reassembled from the spans — not as parsed values, because at this boundary they
have none. `<expr>` uses the canonical expression form of §1.8.

Instructions are rendered by the target, because the source AST is parameterized
over the target's surface-instruction type and generic code has nothing to print.
The rendering is the target's canonical spelling, which is not required to equal
the input spelling: `movl $42,%eax` and `movl $42, %eax` are the same
instruction, and a dump that preserved the difference could not be used to check
that they are.

### 1.3 `--dump-normalized-ast` — after simplify

```text
normalized <unit-name>
label <name>
<directive>
<target-rendered-instruction>
```

Items in input order, minus the metadata-only directives that simplify removes.
`<directive>` is a **normalized** directive value, printed per §3's table
(`align 16`, `globl asm_test_entry`, `size asm_test_entry = . - asm_test_entry`),
so the x86 `.align 16` and the ARM `.balign 4` print as `align 16` and `align 4`:
the same constructor with different values, which is exactly the claim being
made.

### 1.4 `--dump-lowered-ast` — after lowering

```text
lowered <unit-name>
section <name> <perms> align=<n>
  bytes <hex-run> [<form-id>] <fixup>…
  relax
    <hex-run> [<form-id>] <fixup>…
  align <n> fill<=<hex-run>
  zero <n>
  label <name>
  size <name> = <expr>
<visibility> <name> <sym-kind> in <section> [size=<expr>]
declared <name> (not allocated)
```

Fragments in layout order, one per line, indented two spaces under their section.
Sections in input order. Symbols after all sections, in input order. Declared-but-
unallocated sections last.

A `relax` fragment prints one indented line per rung, in ladder order, which is
shortest first. Which rung layout will choose is deliberately absent: this dump
is before layout, and printing a selection here would be printing a guess.

A fixup prints on its own indented line under the fragment that carries it:

```text
@<byte-offset>/<container>B pc+<pc-bias> <name> <kind-name> <range> [<slice>,…] = <expr>
```

where a slice is `<bit-offset>+<bit-width>@<value-lsb>`, in memory order. The
three coordinate systems §5 distinguishes are all on this line, which is the
point of printing it: `byte_offset` and `container` locate the bits, `pc_bias`
locates the PC, and `value_lsb` locates the slice inside the logical value.

Only the *widest* alignment fill is printed, and the `<=` says so: a target
supplies one fill string per padding length, and printing all of them would put
a quadratic byte dump in every aligned section for no added fact.

No addresses appear anywhere in this dump, and that absence is the contract: a
lowered module that carried addresses could not be laid out twice, so if an
address ever appears here it is a bug this dump is designed to expose.

`<perms>` is the three-character `rwx` string with `-` for absent
(`r-x`, `rw-`, `r--`).

### 1.5 `--dump-image` — plan and bound image

Before binding (`plan_image`):

```text
segment <name> size=<n> zero=<n> align=<n> permissions=<perms>
entry <name>
export <name>
unresolved <name>
```

After binding (`bind_image`, i.e. with `--fixed-base`):

```text
section <name> address=<hex> size=<n> permissions=<perms>
entry <hex>
export <name> = <hex> [size=<n>]
```

`unresolved` is what binding still has to do, and it is the other half of what a
plan is for: §9 splits layout from address assignment, so a plan that showed only
what it had already decided would not say why the second step exists. It lists
*outstanding placement-dependent fixups*, not symbols that could not be found —
a fixup naming a symbol the module never defines is rejected during planning
(`image.undefined`), because `bind_image` takes section addresses and has no
import resolver, and reporting it as bindable state would promise something the
API cannot deliver.

`declared` sections appear in `--dump-lowered-ast` (§1.4) and not here, which is
the same rule stated twice: an unallocated section has no address, and this dump
is the one that has addresses in it.

The two forms are deliberately different shapes rather than one shape with empty
address fields. §9 makes address binding the *host's* step; a plan that printed
`address=` at all — even blank — would invite reading it as an address the
assembler chose.

Permissions in both are **metadata**. Nothing in a production package acts on
them.

### 1.6 `--dump-disasm=<mode>` — the disassembler

Two modes, and the distinction is a contract, not a convenience:

- `--dump-disasm=canonical` prints the assembler's own canonical spelling. This
  is what round-trip tests re-parse, so it must be **exactly re-parseable**:
  feeding a canonical dump back through the assembler must produce the identical
  image.
- `--dump-disasm=diagnostic` prints address, encoding bytes and form id
  alongside. This is what a human reads and what the differential gate compares
  against `objdump -dr`. It is **not** required to be re-parseable, and must not
  be re-parsed by any test — a format that has to satisfy both audiences ends up
  satisfying neither.

Diagnostic form:

```text
<address>  <hex-run>  <canonical-spelling>  [<form-id>]
```

with the address printed as bare lowercase hex without `0x` (it is a column, not
a value), the hex run in memory order, and the columns padded so the four fields
line up within one section. Example, aarch64 at base `0x40000000`:

```text
section .text address=0x40000000 size=24 permissions=r-x
40000000  ef 03 00 91  mov x15, sp                [aarch64.add-imm]
40000004  ef 7b bf a9  stp x15, x30, [sp, #-16]!  [aarch64.stp-pre]
40000008  40 05 80 52  movz w0, #42               [aarch64.movz]
4000000c  fe 07 40 f9  ldr x30, [sp, #8]          [aarch64.ldr-uoff]
40000010  ff 43 00 91  add sp, sp, #16            [aarch64.add-imm]
40000014  c0 03 5f d6  ret                        [aarch64.ret]
declared .note.GNU-stack (not allocated)
export asm_test_entry = 0x40000000 size=24
```

The `declared` line comes from `--dump-lowered-ast` and the `section` and
`export` lines from `--dump-image`; the four-column body is
`--dump-disasm=diagnostic`. They are shown together because that is how the CLI
is invoked, not because any of them produces the others.

**Form-forcing spellings are canonical-only.** A decoded x86 branch backed by a
relaxation ladder prints its rung in the mnemonic — `jmp.d8`, `je.d32` — in
`--dump-disasm=canonical` and never in `--dump-disasm=diagnostic`. Every decoded
branch is suffixed, short rungs included, not only the ones a fresh selection
would have spelled differently; §5.6 has the argument for why the rule is
unconditional. The suffix is GAS's own x86 pseudo-suffix, so canonical output
stays assemblable by the reference toolchain, and ordinary source stays
unsuffixed and fully relaxable. The diagnostic dump omits it because that dump is
compared against `objdump`, which has no reason to print it, and because the form
id in the fourth column already says which rung was decoded.

### 1.7 `--dump-codec` — the target description

Not a stage of the pipeline but a rendering of the codec tree itself, which is
why it takes no input. It exists because an instruction description here is a
*value*: if the tree could not be printed, "which forms does this target have,
and where are their fields" would be answerable only by reading the source.

```text
alt <name>
  [<priority> cost=<n>] <label>          <shape>
  [<priority> cost=<n>] <label>          relax <name>
                          <rung-label>   <shape>
<named-shape>(){<shape>}
```

Alternatives in priority order, which is also the order encode tries them and
decode matches them. A `relax` alternative prints its rungs indented beneath it,
in ladder order — shortest first — because that order is what the promotion
fixpoint depends on and a sorted or arbitrary rendering would hide a ladder
declared the wrong way round.

Inside a shape:

```text
0101                      a Const, as its literal bits
<name>:<width><s|u>       a Field
<name>                    a sub-shape, printed once at the end
<name>[<n>]{…}            an Iso_table, with the size of the relation
<<name>:<width>@<lsb> <kind-name>>   a Fixup slice
```

A fixup prints in angle brackets rather than as a field because it is not one:
it emits a placement as well as bits. The `@<lsb>` is which bit of the *logical*
value the slice starts at, so `adrp`'s two slices are visibly one value —
`<page:2@0 adrp-page> … <page:19@2 adrp-page>` — rather than two fields that
happen to share a name. The kind name comes from `TARGET.fixup_kind_name`, the
same function the oracle indexes on, so a dump and a relocation record cannot
disagree about what a kind is called.

### 1.8 Canonical expressions

Expressions appear in four of the seven dumps, so their format is fixed once here.
Fully parenthesized, no precedence-dependent elision, one space around binary
operators, no space after unary:

```text
<int>                     decimal, or 0x-hex if the source wrote hex
<symbol>                  the identifier
.                         the current location
<n>b / <n>f               a numeric local label reference
(<expr> <op> <expr>)      binary
-<expr> / ~<expr>         unary
```

The outermost expression is **not** wrapped, so a bare symbol prints as
`asm_test_entry` and `. - asm_test_entry` prints as `(. - asm_test_entry)`.
Full parenthesization rather than minimal is chosen because minimal
parenthesization has to encode GAS's precedence table into the printer, and then
a bug in the table is invisible in the dump that would otherwise reveal it.

---

## 2. The `form_id` scheme

A `form_id` names **which encoding decision was taken**, so that a differential
test can distinguish "the same bytes" from "the same bytes for the same reason".
Two encoders that agree on `b8 2a 00 00 00` but disagree about whether that is
`mov-imm32-to-eax` or `mov-imm32-to-r32` differ, and the difference will surface
later as a wrong REX prefix or a wrong operand size.

### 2.1 Construction

A `form_id` is the **path of `Alt` labels taken through the target's codec**,
joined with `.`, prefixed by the target name:

```text
<target>.<alt-label>[.<alt-label>]…
```

This is not a hand-maintained table. `Codec.encode` records the label of each
`Alt` it commits to, in order, and `Codec.form_id` joins them. That construction
is the whole point: an id that is derived from the encoding cannot describe a
decision the encoder did not make, whereas a hand-written id can and eventually
does.

### 2.2 Stability rules

1. **Labels are part of the contract.** Renaming an `Alt` label changes the
   `form_id`, which changes every fixture that mentions it. Do it deliberately
   or not at all.
2. **Labels are lowercase, `[a-z0-9-]`, and never contain `.`**, since `.` is the
   separator. Hyphens separate words within one label (`pre-index`), dots
   separate levels of decision.
3. **The path length is not fixed**, and code must not parse a `form_id` by
   position. A target may add an inner `Alt` and lengthen every id under it. Tests
   compare whole ids.
4. **A label names the decision, not the syntax.** `aarch64.add.imm12.sp` is the
   form; that the fixture spelled it `mov x15, sp` is the surface AST's business.
   An id that tracked spelling would change when an alias was added.
5. **Ids are per-target, including within a family.** `x86_32.mov.imm32.eax` and
   `x86_64.mov.imm32.eax` are distinct ids that happen to produce identical bytes.
   Merging them would make the x86-64 dump unable to state that no REX was
   emitted.

### 2.3 The implemented forms

A milestone is defined as the instructions its fixtures emit, and the count is
recorded because it is the one number that says how much of each target exists.
M1 was the four `return42` fixtures: 26 occurrences across 24 distinct forms.
M2 adds `args_arith`, `cond_select`, `direct_call`, `global_ldst` and `loop`, and
the six cases together emit **355 occurrences across 57 distinct forms**:

| Target | M1 forms | M2 forms | Occurrences |
|---|---|---|---|
| `x86_32` | 6 | 17 | 96 |
| `x86_64` | 6 | 18 | 82 |
| `arm` | 7 | 8 | 95 |
| `aarch64` | 5 | 14 | 82 |
| **total** | **24** | **57** | **355** |

The two x86 profiles have roughly twice as many forms as the two fixed-width
targets for a reason that is about the id scheme rather than about the
instruction set: an x86 form id spells out the prefix decisions, so
`lea.asz-absent.rex-present.sib-disp8` and
`lea.asz-present.rex-absent.sib-disp8` are two forms of one instruction. ARM's
eight are correspondingly broad — `arm.dp-imm` covers `add`, `sub`, `and`, `cmp`
and `mov` because A32 encodes them as one data-processing format.

Anything outside this set is out of scope and must be **rejected with a
diagnostic**, not encoded on a guess. §4.3 is the list of what is deliberately
absent.

---

## 3. Per-dialect directive table

Every directive the fixtures contain, and nothing else. **A directive not in
this table is a diagnostic.** An assembler that ignores directives it does not
recognize produces a plausible image from a file it did not understand, which is
the failure mode this table exists to prevent.

Four categories, and the category decides what happens:

| Category | Effect |
|---|---|
| **semantic** | changes what is assembled; becomes a `Directive.t` |
| **declared** | recorded, never allocated; becomes `Declared_section` |
| **target state** | handed to `TARGET.handle_directive`; never reaches generic code |
| **metadata** | arguments consumed lexically, then discarded at simplify |

| Directive | x86_32 | x86_64 | arm | aarch64 | Category | Normalizes to |
|---|:-:|:-:|:-:|:-:|---|---|
| `.text` | ● | ● | ● | ● | semantic | `Section {name = ".text"; perms = r-x}` |
| `.data` | ● | ● | ● | ● | semantic | `Section {name = ".data"; perms = rw-}` |
| `.section <n>,"<f>",<t>` | ● | ● | ● | ● | declared | `Declared_section` when unallocated |
| `.align <n>` | ● | ● | | | semantic | `Align {boundary = n}` — **bytes** |
| `.balign <n>` | | | ● | ● | semantic | `Align {boundary = n}` — **bytes** |
| `.globl <s>` | ● | ● | ● | ● | semantic | `Global {name = s}` |
| `.type <s>, @function` | ● | ● | | ● | semantic | `Sym_type {kind = Function}` |
| `.type <s>, %function` | | | ● | | semantic | `Sym_type {kind = Function}` |
| `.size <s>, <expr>` | ● | ● | ● | ● | semantic | `Sym_size {name; size}` |
| `.syntax unified` | | | ● | | target state | ARM state |
| `.arch <name>` | | | ● | | target state | ARM state |
| `.fpu <name>` | | | ● | | target state | ARM state |
| `.arm` | | | ● | | target state | ARM state |
| `.eabi_attribute <t>, <v>` | | | ● | | target state | ARM state |
| `.cfi_startproc` | ● | ● | ● | ● | metadata | discarded |
| `.cfi_endproc` | ● | ● | ● | ● | metadata | discarded |
| `.cfi_adjust_cfa_offset <n>` | ● | ● | ● | ● | metadata | discarded |
| `.cfi_rel_offset <r>, <n>` | | | ● | ● | metadata | discarded |

**Data directives, where the cell is the width in bytes rather than a tick.**
`.word` is two bytes in GNU x86 syntax and four on ARM and AArch64, so one
universal table would be wrong on two targets, and the width comes from
`TARGET.data_widths` — asked *before* the common table, exactly as
`handle_directive` is asked before it. Each argument stays an `Expr.t`, so a
value naming a symbol becomes a fixup through `TARGET.data_fixup` rather than
being evaluated here.

| Directive | x86_32 | x86_64 | arm | aarch64 | Normalizes to |
|---|:-:|:-:|:-:|:-:|---|
| `.byte` | 1 | 1 | 1 | 1 | `Data {width; values}` |
| `.short` | 2 | 2 | 2 | 2 | `Data {width; values}` |
| `.hword` | | | 2 | 2 | `Data {width; values}` |
| `.word` | **2** | **2** | **4** | **4** | `Data {width; values}` |
| `.4byte` | | | 4 | 4 | `Data {width; values}` |
| `.long` | 4 | 4 | 4 | 4 | `Data {width; values}` |
| `.quad` | 8 | 8 | 8 | 8 | `Data {width; values}` |
| `.xword` | | | | 8 | `Data {width; values}` |

CompCert emits `.4byte` for 32-bit address data on ARM and `.long` on x86, which
is why both spellings are present rather than one canonical name: the table
absorbs the dialect, and nothing downstream of it knows there was one.

Six notes on the rows that are not obvious:

- **`.align` and `.balign` normalize to the same constructor with different
  values.** `.align 16` on x86 and `.balign 4` on ARM both mean "pad to this many
  bytes", and both become `Align`. The dialect difference GAS actually has —
  `.align` meaning a power of two on some targets — does not arise for the four
  targets here, and the table above is the checked claim rather than a general
  one. A target where it does arise states its own unit in its directive handler.
  `.p2align` is the case where the unit genuinely differs, and it is **rejected**
  rather than treated as a synonym: its argument is an exponent, so accepting it
  as a byte count would align `.p2align 4` to four bytes instead of sixteen — a
  valid image at wrong addresses, which is the worst shape a bug can take.

- **`@function` versus `%function` is the *comment introducer* leaking into the
  syntax.** ARM uses `@` for comments, so `@function` would be a comment; GAS
  spells it `%function` there. The lexical profile is what makes this work, and
  it is the clearest small case of why the profile exists.

- **`.section .note.GNU-stack` is declared, never allocated.** This is why the
  section restriction was ever stated as "**allocatable** section" rather than
  "section" — every fixture has two `.section`-ish things and only one of them
  is a section. M1's limit of one allocatable section is gone: `global_ldst`
  puts its object in `.data` and its code in `.text`, and M2 lays out both.
  Merging sections *across inputs*, and linking more than one module, are M3.

- **A NOBITS section is refused**, whether written `.bss` or as a `.section`
  with a `@nobits` or `%nobits` type argument, and the deferral is enforced
  rather than documented (`simplify.nobits-section`, naming M3).
  `Lowered_ast.Zero` already means initialized NUL bytes appended to the buffer,
  which is `.space` in a PROGBITS section — so a NOBITS section taken as
  PROGBITS would get real contents, producing a valid file of the wrong size
  with everything after it at the wrong address. Both spellings of the type are
  parsed for the same reason `@function` has two.

- **`.cfi_*` are metadata, but their arguments are still lexed.** They are
  discarded at simplify, not skipped at parse. Skipping them lexically would mean
  a malformed `.cfi_rel_offset` never produced a diagnostic, and the difference
  between "consumed and discarded" and "never read" is the difference between an
  assembler and a filter.

Also note what the `.cfi_*` discard *costs*: no `.eh_frame` is produced, which is
why the four `return42` fixtures are relocation-free — the only relocations they
would generate are the PC-relative entries in the `.eh_frame` being discarded.
The M2 cases relocate for their own reasons: `direct_call` keeps a call record on
every target, and `global_ldst` a data record (two on the fixed-width targets,
which split one address across two instructions).

---

## 4. Support matrix

Target × capability × runtime. This is the document's answer to "is that
implemented?", and it is required to be **checked**, not asserted: the test suite
reads the same table and fails if a cell marked ● has no test, or if a cell
marked ○ succeeds.

Legend:

| | meaning |
|---|---|
| ● | implemented and tested at this milestone |
| ○ | deliberately not implemented; attempting it is a diagnostic, and that diagnostic is tested |
| ◐ | implemented, tested outside the portable matrix (needs a toolchain or an emulator) |
| — | not applicable in this runtime by construction |

### 4.1 M2 capabilities, native OCaml

| Target | parse | simplify | lower | encode | decode | link | relax | reloc | execute-user | execute-system |
|---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| `x86_32` | ● | ● | ● | ● | ● | ● | ● | ● | ◐ | ○ |
| `x86_64` | ● | ● | ● | ● | ● | ● | ● | ● | ◐ | ○ |
| `arm` | ● | ● | ● | ● | ● | ● | — | ● | ◐ | ○ |
| `aarch64` | ● | ● | ● | ● | ● | ● | — | ● | ◐ | ○ |

`link` is the restricted linker: one module, no imports, no weak or common
symbols. Several allocatable sections in one module are supported at M2; merging
sections across inputs and linking more than one module are M3. Every condition
outside that is an explicit tested rejection, which is why the cell is ● and not
◐ — the restriction is implemented, not merely unimplemented.

`relax` is `—` rather than ○ on the two fixed-width targets: A32 and A64 have no
short and long encoding of one branch, so there is nothing to choose between.
That is a property of the architecture, not a limit of this assembler, and ○ —
which means "attempting it is a tested diagnostic" — would misstate it.

`reloc` is the fixup pipeline end to end: lowering emits a symbolic fixup, layout
carries it, binding patches it, and the differential gate checks the observation
against the measured ELF record. §5.7 is the contract.

`execute-user` is ◐ because it runs the bound image under QEMU user mode via the
checked-in helpers, so it needs cross binutils and four emulator binaries and
lives in `asm-abi-conform` / `asm-exec`, not in `asm-portable`.

`execute-system` is ○ for every target still: the system profile is a separate
non-required workflow and nothing asserts on it yet.

### 4.2 Runtimes

| Capability | native | js_of_ocaml / Node | Melange / Node |
|---|:-:|:-:|:-:|
| parse | ● | ● | ● |
| simplify | ● | ● | ● |
| lower | ● | ● | ● |
| encode | ● | ● | ● |
| decode | ● | ● | ● |
| link (`plan_image`) | ● | ● | ● |
| link (`bind_image`) | ● | ● | ● |
| relaxation and fixups | ● | ● | ● |
| all seven dumps | ● | ● | ● |
| property tests | ● | — | — |
| execute-user | ◐ | — | — |
| execute-system | ○ | — | — |

Every ● in the first three runtime columns is a **byte-identical** claim, not a
"works" claim: M1.7 requires the same dump text and the same image bytes from all
three. The property tests are the one deliberate exception, and their `—` is not
a portability limit: byte-identical output is established by the cram baselines,
which are the same input on all three runtimes, whereas a randomized test is only
the same *distribution* — running it three times would cost three times as much
and prove nothing extra. This is what makes the 31-bit legs meaningful — `linux/i386` and
`linux/arm/v7` have `max_int = 2^30 - 1`, so any place an address was carried in
a native `int` produces a different answer there, and the equality check finds it.

The `—` cells are by construction and not by omission: executing generated code
requires allocating executable pages, which §9 places outside every production
package and which a browser runtime cannot do at all. The Melange and jsoo
columns are the direct evidence that the production closure has no such
dependency.

### 4.3 What is deliberately absent

Recorded here so that "not in the matrix" never has to be interpreted:

- multi-module linking, and section merging *across inputs* (M3). Several
  allocatable sections within **one** module are supported: the M2 global
  fixture puts its object in `.data` and its code in `.text`
- `.bss`, `@nobits`/`%nobits` sections, and any uninitialized storage
  reservation (M3). Rejected rather than ignored — `section_of_args` parses the
  section type argument precisely so a NOBITS input cannot slip into the
  PROGBITS model and silently gain initialized bytes
- `.comm` and `.local` (M3, with the rest of common allocation). CompCert emits
  them only for uninitialized globals, so an initialized fixture avoids them
- weak and common symbols, strong/weak resolution (M3)
- imports: a fixup naming a symbol the module does not define is rejected during
  planning, because `bind_image` takes section addresses and has no import
  resolver. Accepting one would report bindable state the API cannot satisfy
- symbol assignment (`name = expr`), rejected at simplify: it needs a symbol
  flavour the lowered AST does not have, and no CompCert output uses it
- `.p2align`, rejected rather than treated as a synonym for `.balign`, because
  its argument is an exponent. No fixture emits one; when one does, it is
  normalized rather than listed, with a portable maximum exponent of 29 — the
  31-bit `int` on the `linux/i386` and `linux/arm/v7` legs cannot represent
  `1 lsl 30` as a positive `int`
- `.include`, conditional assembly, repetition, user macros (§4.10, later)
- any form of object-file output — the lowered module carries what an object file
  would carry precisely so that none is ever written

Left the list in M2: fixups and relocations, branch relaxation, PC-relative and
symbolic operand expressions, numeric local labels, relocation modifiers
(`:lo12:`, `:lower16:`, `:upper16:`), ARM `movw`/`movt`, and several allocatable
sections in one module.

## 5. The fixup and relaxation contract

Frozen at M2. Four targets have to agree on every clause here, and each one is
a thing that was ambiguous until it was written down.

### 5.1 What a fixup names

A fixup records **two different locations**, and conflating them is the mistake
the shape exists to prevent:

- `byte_offset` + `container` — where the bits go. The container is read and
  written **little-endian**; all six targets are little-endian on the wire
  (`exec-abi-v1.md` §1), so one patch routine serves x86's byte-aligned
  `disp32`, ARM's 24-bit field inside a word, and AArch64's split `adrp`
  immediate.
- `pc_bias` — what the architecture measures a displacement *from*. The PC base
  is `fragment address + pc_bias`.

On x86 these differ by the whole instruction: a `call rel32` patches at offset 1
and measures from offset 5. `pc_bias` is therefore the **realized** encoded
length, computed by `encode` from the form it actually produced — not a
per-mnemonic constant, because a RIP-relative load is six bytes without a REX
prefix and seven with one. ARM is a constant 8 (the A32 pipeline offset) and
AArch64 a constant 0.

`evaluate_fixup` receives an **already biased** `place` and adds nothing of its
own. Scaling and per-kind range checks stay in the target.

### 5.2 Slices

A logical value may occupy several non-adjacent runs — AArch64 `adrp`'s
`immlo`/`immhi`, ARM's `movw`/`movt` — so a fixup carries a list of slices, each
saying which bit of the *value* it starts at. In the codec these are separate
`Fixup` nodes sharing a name; `encode` merges them into one placement.
Reassembling the pieces, and applying signedness **once to the whole**, is the
target's `Iso_fun`. Applying it per slice sign-extends a fragment of a number
and is the bug this arrangement makes impossible to write by accident.

`Codec.check` requires same-named slices to share a kind (compared by
`equal_fixup_kind`, never by printed name), not to overlap in value space, and
to cover the value contiguously from bit 0.

### 5.3 Ranges

The accepted values of a fixup are a three-case type, not a signedness:

| range | accepts |
|---|---|
| `Signed n` | `[-2^(n-1), 2^(n-1))` |
| `Unsigned n` | `[0, 2^n)` |
| `Bitpattern n` | `[-2^(n-1), 2^n)` |

`Bitpattern` exists because a 32-bit absolute address is neither: `Signed 32`
rejects every address at or above `0x80000000`, and `Unsigned 32` rejects a
negative displacement. The range is checked **at bind**, against the whole
logical value — at encode time every symbolic value is a placeholder that
trivially fits.

Patching **overwrites** the slice bits; it never adds. Addends live in the
expression, so applying a patch twice cannot double a displacement.

### 5.4 `here` and `unresolved`

`here` inside an operand expression is the instruction's address, matching GAS.
`plan.unresolved` lists **placement-dependent fixups still outstanding** —
locally defined symbols awaiting an address — which is what §9 says a plan
reports and what binding satisfies. It is not a list of missing symbols; those
are rejected during planning (§4.3).

### 5.5 Relaxation ladders

A `Relax` fragment holds several encodings of one operation, **shortest first**.
It is a distinct codec node rather than an `Alt` because an `Alt` dispatches
between different things and returns the first success, while a ladder must hand
back every rung at once and later be told which to use.

Ladder compatibility is checked on the fixup's **family**, not its kind: an x86
short/near pair necessarily changes kind, width and `pc_bias`, which is what a
ladder is. Every rung must carry the same fixup names, the same expressions, and
the same families, with strictly increasing sizes.

Rungs must be **decode-distinguishable** — `check` rejects overlapping fixed
bits. Two semantically equivalent but indistinguishable rungs would make the
first always match, losing which rung produced an encoding.

**Nesting is rejected, not defined.** `check` refuses a ladder inside a ladder,
and refuses two ladders sequenced on one realized path. Either shape would make
"the rungs of this form" a product of independent choices while `encode_ladder`
returns a list, and the reading it got would depend on the order the walk
happened to take.

Selection is a **promotion-only fixpoint**: start at the shortest rung, lay the
section out, promote anything that does not reach, repeat. Selections travel one
way along a finite ladder, so it terminates; after convergence every selection
is verified and `image.relax-unreachable` reports one that still does not reach.

Two limits are deliberate. Relaxation is **intra-section only** — a displacement
within one section is independent of where the section is placed, which is what
makes it decidable at plan time; a cross-section target keeps the longest rung.
And the result is **not claimed minimal**: alignment padding makes layout
non-monotone in fragment size, so a promotion whose bytes are absorbed by
padding is not undone. What establishes agreement with GNU is the byte-for-byte
differential gate on each fixture, not an optimality argument.

### 5.6 Which interpreter, and why the pin is unconditional

The choice of interpreter is made on the **lowered operand**, in the target,
before the codec is reached. It has to be: a resolved displacement that fits a
short rung also fits a long one, so "several rungs succeeded" cannot tell a
relaxable branch from a pinned one, and the codec should not have to guess.

| lowered operand | codec operation | result |
|---|---|---|
| `Symbolic { rung = None }` | `encode_ladder` | `` `Relax`` (or `` `Fixed`` if one rung applies), placeholder bits, expression-bearing fixups |
| `Symbolic { rung = Some r }` | `encode_rung ~rung:r` | `` `Fixed``, placeholder bits, **and a fixup** |
| `Resolved { value; rung }` | `encode_rung ~rung` | `` `Fixed``, real bits, **no** fixups |

Three states rather than two, because a pin outlives resolution: a branch parsed
back from canonical `jmp.d32` text is pinned *and still symbolic* — its printed
target is an address-dependent expression until binding — so it must keep a
fixup and must not rebuild the ladder.

A pin is obeyed or diagnosed, never quietly replaced, and **which phase
diagnoses it is frozen per input path** so the two cannot drift into each other:

| input | phase | diagnostic |
|---|---|---|
| text — `jmp.d16 foo` | the target's mnemonic parser, which knows the suffix set and has a source span | `x86.branch-suffix` |
| programmatic — a direct-lowered `Symbolic { rung = Some "bogus" }` | `Codec.encode_rung`, the only layer that can see an unknown rung name | `codec.unknown-rung` |

A declared rung that cannot take the value is `codec.rung-inapplicable`, which is
a different mistake — about the value rather than about the description.

**Canonical disassembly pins every decoded x86 branch backed by a ladder**,
short rungs included. Not only the non-minimal ones, for two reasons. It is
underspecified otherwise: `Instruction.pp` has no address context in which to
run "what would a fresh selection choose", and the short and near rungs have
different `pc_bias`, so the answer is undefined exactly at the boundary. And it
is unsound: an unsuffixed short spelling re-parses to
`Symbolic { value = Const T; rung = None }`, whose `Const T` names no symbol and
no section, so the intra-section rule cannot decide it and its displacement is
unknown until binding. Treating it as cross-section picks the longest rung and
changes the bytes; guessing the shortest can fail at bind, after promotion is
over. There is no address-independent correct unpinned choice, so the pin is
never discarded. Ordinary source stays unsuffixed and fully relaxable; only
canonical output is pinned, which is what its byte-exactness contract asks for.

This machinery is **x86-only**: ARM and AArch64 are fixed-width, declare no
ladder, and never carry a preference.

### 5.7 What the oracle sees

Layout erases the fixup-kind type — the evaluator is partially applied at plan
time so `laid_out` stays unparameterized and the architecture-erased driver keeps
its shape — so a kind-erased **observation** is retained per fixup and exposed as
`Image.fixup_observations`. Without it the relocation gate, which enters through
the erased boundary, could not see its own inputs.

Two variants, not one record with optional fields:

- `Symbolic_ref` — a site (section, container offset, kind name, family, role)
  plus `symbol`, `addend`, `binding`, `visibility`, `defined` and `same_section`.
- `Non_normalizable` — a site and the printed expression. An expression naming
  two symbols has no symbol, and therefore no binding, visibility or section
  relation either; making those optional would be representing a state that
  cannot occur.

Every field the oracle indexes on is a **separate column**, sourced from the
lowered module's own symbol table. Nothing recovers a role by parsing a kind
name, so changing the role, the binding, the section relation or the visibility
independently changes an observation and fails a comparison.

**Internal fixups and ELF records are not the same set.** GNU resolves many
same-section references while assembling; this assembler needs a fixup to lay
out, relax, bind and patch a branch that has no record at all. So the gate
classifies before it compares, per target and per fixup kind, into
assembler-resolved or linker-visible-with-a-specific-type. The obvious rules are
measurably wrong in both directions: a defined *global* same-section branch is
assembler-resolved on both x86 profiles, so binding is not decisive, and a call
to a global is `R_X86_64_PLT32` on x86-64 but plain `R_386_PC32` on x86-32, so
the ELF spelling is not uniform. Every cell comes from a committed oracle
artifact for a case that exercises it.

Records are compared as **keyed multisets**, keyed on section and offset, never
positionally: order is a traversal artifact that a layout refactor would change
without changing the program. A duplicate key is itself reported rather than
allowed to decide silently which of two records a lookup finds, and missing,
extra and mismatched are three distinct findings.

Addends are derived per class, because only the PC-relative classes subtract a
place:

| class | transform |
|---|---|
| PC-relative | `A = expr_addend + byte_offset − pc_bias` |
| absolute, page-relative, scaled-low-12, ARM split | `A = expr_addend` |

x86-32 and ARM are REL, so the addend lives in the field and is recovered per
kind — an ARM split immediate needs the same slice extraction the patcher uses.
x86-64 and AArch64 are RELA. A split form produces **one record per
instruction**, not per slice: `adrp` and `ldr :lo12:` are two records at two
offsets matching two fixups, and §5.2's slices live *within* one record.
