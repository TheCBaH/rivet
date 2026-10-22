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
- `v` implies `zve64d`; `zve64d` implies `zve64f`; `zve64f` implies `zve32f`
  and `zve64x`.
- `zvkn`, `zvks` and their sums with `zvbc`/`zvkg` imply the vector crypto
  leaves; `zvbb` implies `zvkb`; `zvknhb` implies `zvknha`.
- `zfh` implies `zfhmin`.
- `zvfbfwma` requires `zvfbfmin` and `zfbfmin`, so enabling it enables both.

The groups are off by default so the leaves they imply can still be removed one
at a time. A form shared by several leaves carries an `Any` predicate over the
leaves only: `andn`, `orn`, `xnor`, `rol`, `ror`, `rolw`, `rorw`, `rori`, `roriw`,
`rev8` need `zbb` or `zbkb`; `clmul`, `clmulh` need `zbc` or `zbkc`;
`aes64ks1i`, `aes64ks2` need `zknd` or `zkne`; the nine vector forms Zvkb shares
with Zvbb need `zvbb` or `zvkb`; `flh`, `fsh`, `fmv.x.h`, `fmv.h.x` need `zfhmin`
or `zfbfmin` (riscv-opcodes lists them under Zfhmin only).

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

## Changing the set in the source

`.option arch, <item>[, <item>...]` edits the enabled set from that point on.
Each item is `+<ext>` or `-<ext>` with the feature names of the table below,
case-insensitive, optionally followed by a GNU version (`+zba2p0`; the version
is accepted and ignored, as one version of each extension is modelled). Items
apply left to right with the same rules as `--features`
(`Target_config.apply`): requirements are enabled, implications re-closed
without re-adding a removed feature, and only the final set is validated, so
`.option arch, -c, -zcd, +zcmp` is one valid step. A rejected directive leaves
the set unchanged. `.option push`/`pop` save and restore the set together with
the pic, relax and rvc flags. An unsigned item, an unknown or inapplicable
feature, or a contradictory result is an error.

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

## Vector

Every V, Zvbb, Zvbc, Zvfbfmin, Zvfbfwma and Zvk* form is a generated table row,
so the vector instructions encode and decode through one description. Operands
follow GNU order: the destination (`vd`, a store's `vs3`, or `rd`), then the
address with its stride or index for a load or store, otherwise `vs2` and the
second source. The multiply-add families (`vmacc`, `vfmacc`, `vwmacc*`,
`vfwmaccbf16`, ...) spell the multiplier first, `vd, vs1, vs2`. The mask is an
optional trailing `v0.t`; a `.vvm`/`.vxm`/`.vim`/`.vfm` form with a fixed
zero mask spells it `v0`. The scalar of a `.vf`/`.wf` form is an FPR.

`vsetvli` and `vsetivli` take vtype as the keyword list `e<sew>, m<lmul>,
ta|tu, ma|mu`: any non-empty subset in that order, an omitted category being
zero. Decoding prints all four keywords; a reserved vtype (SEW above 64, LMUL
code 4, or bits above 7) has no spelling and does not decode.

A unit-stride, strided or indexed load or store has a variable `nf` field.
Its record is the single-field form (nf = 0, part of the row's mask) and the
generator adds the segment forms with two to eight fields, spelled `seg<n>`
after the addressing prefix: `vlseg2e8.v`, `vlsseg3e16.v`, `vluxseg4ei32.v`,
`vsoxseg8ei64.v`. Whole-register loads and stores keep their own records.

The rows tagged `v` belong to the embedded profiles, split the way GNU as gates
them: the floating-point forms (`vf*` and `vmf*`, except the integer
`vfirst.m`) to `zve32f`, loads and stores of 64-bit elements (width field 7) to
`zve64x`, and the rest to `zve32x`. `zve64f`, `zve64d` and `v` add no encodings;
SEW=64 arithmetic and double-precision elements are a `vtype` matter at run time.
Without `zve64x` ELEN is 32, so `vsetvli` and `vsetivli` refuse `e64` (it would
set `vill`) and `mf8` (reserved); `vsetvl` takes vtype from a register and is
not checked.
`zve64d` requires `d`, so a core without D names its profile explicitly, as
Coral NPU's `rv32imf_zve32f_zicsr_zifencei_zbb_zfbfmin_zvfbfmin_zvfbfwma`
becomes

    --features none,+m,+f,+zicsr,+zifencei,+zbb,+zve32f,+zfbfmin,+zvfbfmin,+zvfbfwma

Removing `v` alone keeps the profiles it implied, as removing `c` keeps `zca`.
The crypto leaves require `zve32x` (`zvbc` and `zvknhb` `zve64x`), as the
specification says; GNU as 2.43 does not check it.

## ISA strings

`Riscv_family_encode.Riscv_isa` converts between configurations and ISA
strings. `of_march "rv32imf_zve32f_zvfbfwma"` gives the XLEN and a spec starting
from `none`, with `system` and `s` on (GNU as takes their instructions under any
`-march`). Versions are accepted and ignored, `g` expands to
`imafd_zicsr_zifencei`, and `zvl<n>b` is returned as `vlen`, since no feature
models it. Unknown names fail in `Target_config.resolve`; Zaamo and Zalrsc are
unknown because A is one component.

`to_string ~xlen config` writes the resolved set in canonical order with
ratified versions (`?isa_spec:`V2_2` uses 2.2's and folds Zicsr and Zifencei
into I), adding the `zvl<n>b` the vector profile and `?vlen` imply. It writes
Rivet's closure, which is not quite either assembler's: C brings in `zca` (and
`zcf`, `zcd`) and Zvknhb implies Zvknha, which LLVM 19 leaves out; LLVM adds
`zkr`, `zkt` to Zk and `zvkt` to Zvkn, and GNU as 2.44 adds `zaamo`, `zalrsc`
to A. `test_features.ml` pins the output against LLVM 19.

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
- every row is 32 bits wide and has a `match_` within its `mask`;
- no row overlaps a standard generated row of the XLEN. The hand-written forms
  are code rather than masks, so for them the standard decoder must decline the
  row's `match_` with its free bits clear, all set, and each set alone. A row
  may therefore lie in the custom-0..3 opcodes or in space the standard
  extensions leave reserved, such as the draft Zvt tile instructions in OP-V,
  LOAD-FP/STORE-FP with `mew` set, and OP-VE;
- no row repeats a mnemonic of a hand-written or standard form, no two rows
  overlap, every row's feature is a vendor component, and every row is listed by
  a vendor component.

Vendor rows use the same operand kinds as the generated ones. `Indexed` spells
a numbered name such as the matrix tile `mt3`: `<prefix>N` in canonical
decimal, encoded as `N / scale` in a field, with `N` required to be a multiple
of `scale`. Its prefix must not begin a register name.

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
| zfhmin | f |  |  | yes | 10 | flh: one of zfhmin, zfbfmin; fsh: one of zfhmin, zfbfmin; fmv.x.h: one of zfhmin, zfbfmin; fmv.h.x: one of zfhmin, zfbfmin; fcvt.d.h: zfhmin and d; fcvt.h.d: zfhmin and d; fcvt.h.q: zfhmin and q; fcvt.q.h: zfhmin and q |
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
| zve32x | zicsr |  |  | yes | 450 |  |
| zve32f | zve32x, f |  |  | yes | 101 |  |
| zve64x | zve32x |  |  | yes | 76 |  |
| zve64f |  | zve32f, zve64x |  | yes | 0 |  |
| zve64d | d | zve64f |  | yes | 0 |  |
| v |  | zve64d |  | yes | 0 |  |
| zvbb | zve32x | zvkb |  | yes | 16 | vandn.vv: one of zvbb, zvkb; vandn.vx: one of zvbb, zvkb; vbrev8.v: one of zvbb, zvkb; vrev8.v: one of zvbb, zvkb; vrol.vv: one of zvbb, zvkb; vrol.vx: one of zvbb, zvkb; vror.vv: one of zvbb, zvkb; vror.vx: one of zvbb, zvkb; vror.vi: one of zvbb, zvkb |
| zvkned | zve32x |  |  | yes | 11 |  |
| zvbc | zve64x |  |  | yes | 4 |  |
| zvknha | zve32x |  |  | yes | 3 |  |
| zvksed | zve32x |  |  | yes | 3 |  |
| zvkg | zve32x |  |  | yes | 2 |  |
| zvksh | zve32x |  |  | yes | 2 |  |
| zvfbfmin | zve32f |  |  | yes | 2 |  |
| zvfbfwma | zve32f, zfbfmin, zvfbfmin |  |  | yes | 2 |  |
| zvkb | zve32x |  |  | yes | 0 |  |
| zvknhb | zve64x | zvknha |  | yes | 0 |  |
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
| c |  | zca, zcd if d |  | yes | 0 |  |

## Not covered

The psABI, full GNU ISA-string intake (`-march=` and `.option arch, =<ISA>`),
`.attribute arch` (still consumed and ignored), `.insn` and runtime plugin
loading are outside this model.
