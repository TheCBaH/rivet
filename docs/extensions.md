# RISC-V extensions

How the RISC-V encoder decides which instructions exist. The generic mechanism
(components, features, the resolver, the gate) is specified in
[contracts.md §6](contracts.md); this page records the RISC-V inventory and the
rules specific to it.

## Model

`Target_component.t` and `Target_config` are the single authority for legality.
A form has an **owner** (the component that lists it, catalogue provenance only)
and a **predicate** (`Req`: `Feature f`, `All [...]` or `Any [...]`) that must hold
over the enabled set for the form to be assembled, encoded or decoded. The
base integer instruction set is not a component: it is always available, even
under `none`.

Each hand-written form and each generated table row has exactly one owner. A
test fails if a mnemonic has none, other than the listed base ones.

## Implications instead of alternatives

The K suites are sums of leaf extensions, so they are implications, not extra
alternatives on every form:

- `zk` implies `zkn`; `zkn` implies `zbkb`, `zbkc`, `zbkx`, `zkne`, `zknd`,
  `zknh`; `zks` implies `zbkb`, `zbkc`, `zbkx`, `zksed`, `zksh`.
- `b` implies `zba`, `zbb`, `zbs`.
- `zvkn`, `zvks` and their sums with `zvbc`/`zvkg` imply the vector crypto
  leaves; `zvbb` implies `zvkb`; `zvknhb` implies `zvknha`.
- `zfh` implies `zfhmin`.

The groups are off by default so the leaves they imply can still be removed one
at a time. A form shared by several leaves carries an `Any` predicate over the
leaves only: `andn`, `orn`, `xnor`, `rol`, `ror`, `rolw`, `rorw`, `rori`, `roriw`,
`rev8` need `zbb` or `zbkb`; `clmul`, `clmulh` need `zbc` or `zbkc`;
`aes64ks1i`, `aes64ks2` need `zknd` or `zkne`; the nine vector forms Zvkb shares
with Zvbb need `zvbb` or `zvkb`.

## Forms with a predicate beyond their owner

| form | owner | predicate |
|---|---|---|
| `amocas.b`, `amocas.h` (and `.aq`/`.rl`/`.aqrl`) | `zacas` | `zacas` and `zabha` |
| Zfa forms on `.d`, `.q`, `.h` | `zfa` | `zfa` and the format's extension (`d`, `q`, `zfh`) |
| `fmvh.x.d`, `fmvp.d.x`, `fcvtmod.w.d` | `zfa` | `zfa` and `d` |
| `fmvh.x.q`, `fmvp.q.x` | `zfa` | `zfa` and `q` |
| `fcvt.d.h`, `fcvt.h.d` | `zfhmin` | `zfhmin` and `d` |
| `fcvt.h.q`, `fcvt.q.h` | `zfhmin` | `zfhmin` and `q` |
| `c.mul` | `zcb` | `zcb` and `zmmul` |
| `c.sext.b`, `c.sext.h`, `c.zext.h` | `zcb` | `zcb` and `zbb` |
| `c.zext.w` | `zcb` | `zcb` and `zba` |
| `c.ntl.*` | `zihintntl` | `zihintntl` and `zca` |
| `c.sspush`, `c.sspopchk` | `zicfiss` | `zicfiss` and `zcmop` |

## Compressed

`c` is an implication-only group: it implies `zca`, `zcd` when `d` is enabled
and, on RV32 only, `zcf` when `f` is enabled. The generated rows tagged `c` are
split by the format they move (double: `zcd`; single: `zcf`; the rest: `zca`).
`.option rvc` stays a separate permission flag: a compressed mnemonic needs both
the extension and `.option rvc`.

Zcmp and Zcmt reuse the double-precision compressed load/store encodings, so
both conflict with `zcd` and are off by default. To use them, drop C's `zcd`:

    --features -c,-zcd,+zcmp
    --features -c,-zcd,+zcmt

(`zca` stays enabled, so the integer compressed forms remain.) On either XLEN,
`0xb842` decodes as `c.fsdsp` by default and as `cm.push` under the first
recipe. This replaces the former behavior of accepting `cm.*` by default.

## Row overlaps

Thirty-three pairs of generated rows have overlapping encodings. Each is
classified in `Riscv_overlap` and a test fails on an unclassified pair:

- *specialization* (15): the first form is a special case of the second (fixed
  registers, or equal source registers), such as `zext.w` and `add.uw`, `fabs.q`
  and `fsgnjx.q`, `sspush` and `mop.rr.7`. Rows are tried in table order, so the
  specialization comes first and wins when both are legal.
- *disjoint operands* (11): the masks overlap but a reserved operand value keeps
  the decoded sets apart, such as `c.addi16sp` and `c.lui`.
- *exclusive by configuration* (7): the providing extensions conflict (`cm.*`
  and `cm.jalt` against `c.fsdsp`), so no valid configuration enables both.

## Vendor extensions

`Extensions.make ~xlen definition` composes a vendor's components and rows with
the standard ones and validates the result once; it reports every problem it
finds. `Riscv_family.Make_with (Profile) (E)` and `Riscv_family_encode.Make_with`
then build a front end and an encoder from the validated composition, and `Make`
is `Make_with` over the standard extensions. Applying a composition validated for
a different XLEN raises `Invalid_argument`. Nothing is registered globally: a
program can hold several compositions side by side, each with its own driver.

A vendor definition must satisfy:

- its components are off by default and appear after the standard ones;
- every row lies in the custom-0..3 major opcode space (`0x0b`, `0x2b`, `0x5b`,
  `0x7b`) with a full 7-bit opcode in its mask, is 32 bits wide, and has a
  `match_` within its `mask`;
- no row repeats a mnemonic of a hand-written or standard form, no two rows
  overlap, every row's feature is a vendor component, and every row is listed by
  a vendor component.

## Components

The 64-bit profile's components; the 32-bit profile differs only in `zcf`
being applicable. "forms" counts hand-written and generated forms the component
owns; the last column lists forms whose predicate is not just the feature.

| feature | requires | implies | conflicts | default | forms | predicates |
|---|---|---|---|---|---|---|
| zmmul |  |  |  | yes | 5 |  |
| m | zmmul |  |  | yes | 8 |  |
| f | zicsr |  |  | yes | 43 |  |
| d | f |  |  | yes | 35 |  |
| a |  |  |  | yes | 22 |  |
| zacas | a |  |  | yes | 20 | amocas.b: zacas and zabha; amocas.b.aq: zacas and zabha; amocas.b.rl: zacas and zabha; amocas.b.aqrl: zacas and zabha; amocas.h: zacas and zabha; amocas.h.aq: zacas and zabha; amocas.h.rl: zacas and zabha; amocas.h.aqrl: zacas and zabha |
| zabha | a |  |  | yes | 72 |  |
| q | d |  |  | yes | 35 |  |
| zfh | f | zfhmin |  | yes | 29 |  |
| zfhmin | f |  |  | yes | 10 | fcvt.d.h: zfhmin and d; fcvt.h.d: zfhmin and d; fcvt.h.q: zfhmin and q; fcvt.q.h: zfhmin and q |
| zfa | f |  |  | yes | 33 | fmvh.x.q: zfa and q; fcvtmod.w.d: zfa and d; fli.d: zfa and d; fli.q: zfa and q; fli.h: zfa and zfh; fmvh.x.d: zfa and d; fround.d: zfa and d; froundnx.d: zfa and d; fround.q: zfa and q; froundnx.q: zfa and q; fround.h: zfa and zfh; froundnx.h: zfa and zfh; fmvp.q.x: zfa and q; fleq.d: zfa and d; fltq.d: zfa and d; fmaxm.d: zfa and d; fminm.d: zfa and d; fleq.q: zfa and q; fltq.q: zfa and q; fmaxm.q: zfa and q; fminm.q: zfa and q; fleq.h: zfa and zfh; fltq.h: zfa and zfh; fmaxm.h: zfa and zfh; fminm.h: zfa and zfh; fmvp.d.x: zfa and d |
| zfbfmin | f |  |  | yes | 2 |  |
| zba |  |  |  | yes | 9 |  |
| zbb |  |  |  | yes | 24 | andn: one of zbb, zbkb; orn: one of zbb, zbkb; xnor: one of zbb, zbkb; rol: one of zbb, zbkb; ror: one of zbb, zbkb; rev8: one of zbb, zbkb; rolw: one of zbb, zbkb; rorw: one of zbb, zbkb; rori: one of zbb, zbkb; roriw: one of zbb, zbkb |
| zbs |  |  |  | yes | 8 |  |
| zbc |  |  |  | yes | 3 | clmul: one of zbc, zbkc; clmulh: one of zbc, zbkc |
| zbkb |  |  |  | yes | 6 |  |
| zbkc |  |  |  | yes | 0 |  |
| zbkx |  |  |  | yes | 2 |  |
| zknd |  |  |  | yes | 7 | aes64ks2: one of zknd, zkne; aes64ks1i: one of zknd, zkne |
| zkne |  |  |  | yes | 4 |  |
| zknh |  |  |  | yes | 14 |  |
| zksed |  |  |  | yes | 2 |  |
| zksh |  |  |  | yes | 2 |  |
| zicond |  |  |  | yes | 2 |  |
| b |  | zba, zbb, zbs |  | no | 0 |  |
| zkn |  | zbkb, zbkc, zbkx, zkne, zknd, zknh |  | no | 0 |  |
| zks |  | zbkb, zbkc, zbkx, zksed, zksh |  | no | 0 |  |
| zk |  | zkn |  | no | 0 |  |
| zicsr |  |  |  | yes | 13 |  |
| zifencei |  |  |  | yes | 1 |  |
| zicntr | zicsr |  |  | yes | 6 |  |
| zihintntl |  |  |  | yes | 8 | c.ntl.all: zihintntl and zca; c.ntl.p1: zihintntl and zca; c.ntl.pall: zihintntl and zca; c.ntl.s1: zihintntl and zca |
| zawrs |  |  |  | yes | 2 |  |
| zimop |  |  |  | yes | 40 |  |
| zicfiss |  |  |  | yes | 13 | c.sspopchk: zicfiss and zcmop; c.sspush: zicfiss and zcmop |
| zicfilp |  |  |  | yes | 1 |  |
| zicbom |  |  |  | yes | 3 |  |
| zicboz |  |  |  | yes | 1 |  |
| zicbop |  |  |  | yes | 3 |  |
| system |  |  |  | yes | 2 |  |
| s |  |  |  | yes | 2 |  |
| h | s |  |  | yes | 15 |  |
| svinval |  |  |  | yes | 5 |  |
| sdext |  |  |  | yes | 1 |  |
| ssctr |  |  |  | yes | 1 |  |
| v | d |  |  | yes | 375 |  |
| zvbb | v | zvkb |  | yes | 16 | vandn.vv: one of zvbb, zvkb; vandn.vx: one of zvbb, zvkb; vbrev8.v: one of zvbb, zvkb; vrev8.v: one of zvbb, zvkb; vrol.vv: one of zvbb, zvkb; vrol.vx: one of zvbb, zvkb; vror.vv: one of zvbb, zvkb; vror.vx: one of zvbb, zvkb; vror.vi: one of zvbb, zvkb |
| zvkned | v |  |  | yes | 11 |  |
| zvbc | v |  |  | yes | 4 |  |
| zvknha | v |  |  | yes | 3 |  |
| zvksed | v |  |  | yes | 3 |  |
| zvkg | v |  |  | yes | 2 |  |
| zvksh | v |  |  | yes | 2 |  |
| zvfbfmin | v |  |  | yes | 2 |  |
| zvfbfwma | v |  |  | yes | 2 |  |
| zvkb | v |  |  | yes | 0 |  |
| zvknhb | v | zvknha |  | yes | 0 |  |
| zvkn |  | zvkned, zvknhb, zvkb |  | no | 0 |  |
| zvks |  | zvksed, zvksh, zvkb |  | no | 0 |  |
| zvknc |  | zvkn, zvbc |  | no | 0 |  |
| zvkng |  | zvkn, zvkg |  | no | 0 |  |
| zvksc |  | zvks, zvbc |  | no | 0 |  |
| zvksg |  | zvks, zvkg |  | no | 0 |  |
| zca |  |  |  | yes | 34 |  |
| zcd | d, zca |  |  | no | 4 |  |
| zcf | f, zca |  |  | no | 4 |  |
| zcb | zca |  |  | yes | 13 | c.zext.w: zcb and zba; c.sext.b: zcb and zbb; c.sext.h: zcb and zbb; c.zext.h: zcb and zbb; c.mul: zcb and zmmul |
| zcmp | zca |  | zcd | no | 6 |  |
| zcmt | zca |  | zcd | no | 1 |  |
| zcmop | zca |  |  | yes | 8 |  |
| c |  | zca, zcd if d |  | yes | 0 |  | |}]

## Not covered

The psABI, full GNU ISA-string intake (`-march=`), `.option arch`, `.insn` and
runtime plugin loading are outside this model.
