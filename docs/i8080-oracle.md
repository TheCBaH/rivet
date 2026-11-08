# Intel 8080: oracles, dialect and behavioural model

The i8080 target is checked against binaries Rivet does not build. They come
from a published `devcontainer.retrocpu-tools` release, pinned by tag and
SHA-256 in `scripts/retrocpu.pin` and fetched by `scripts/retrocpu-fetch.sh`
(which refuses a file whose hash differs and unpacks nothing from it): `asl` and
`p2bin` (Macro Assembler AS), `altairz80` (SIMH, `set cpu 8080`) and
`vm80a_sim` (Verilator model of the KR580VM80A). `make fixture-oracle-i8080`
fetches and runs every comparison below; it is its own CI leg (x86_64 and
arm64) and not part of `make ci`. `make i8080-oracle` alone skips with a message
when nothing is fetched.

Expected bytes come from `asl`, expected execution from `altairz80` and
`vm80a_sim`; neither is derived from Rivet's descriptor table.

## The AS dialect (`I8080_as`)

`targets/i8080/i8080_as.ml` reads the dialect of AS for `cpu 8080`, over the
target's encoder (`I8080_encode`), which stays the only list of opcodes:
`cpu`, `org` (several), `equ`, `set`/`=`, `db`, `dw`, `ds`, `dup`, `end`,
`error`, `macro`/`endm` with `$$` locals, `rept`, `if`/`else`/`elseif`/`endif`/
`ifdef`/`ifndef`, `$`, the literal forms `NNh`, `NNb`, `NNo`/`NNq`, `NNd`,
`0xNN`, `$NN`, characters and strings, and AS's operators with AS's
priorities (measured against `asl`: comparisons < `&&` `||` < `+` `-` < `*` `/`
`#` < `^` < `!` < `|` < `&` < shifts; unary minus binds like `^`). `low x`,
`high x` and `low(x)`, `high(x)` are accepted (AS has neither). Anything else
is reported as `file:line: unsupported: ...`, with the macro expansion chain,
and assembly continues, so one run lists every unsupported construct. `$` is the
address of the statement. The result is a list of segments (one per contiguous
run), a symbol table and a listing map; `flatten` cuts the bytes of a range
like `p2bin -l 0 -r`.

Not supported: M80 syntax (`.8080`, `aseg`, `local`, `&` concatenation,
`<>` macro arguments, `ds n,fill`), `include`, listing directives other than
ignored `title`/`page`/`list`.

## Comparisons

| Script | Compares |
| --- | --- |
| `i8080-corpus.sh`, `i8080-oracle-compare.sh` | 244 documented forms x operand boundary values, symbols, expressions of every ordered operator pair, 42 refused inputs: bytes from `asl` + `p2bin` and from Rivet, classified differences in `test/i8080/oracle-known.txt` |
| `i8080-oracle-sources.sh` | AS-syntax `8080PRE`, `8080EX1` and the K5 shim assembled by both; the M80 originals are refused with positions |
| `i8080-oracle-model.sh` | `8080PRE` and `8080EX1` in the model through the K5 shim, console text equal to `altairz80` |
| `i8080-oracle-dis.sh` | all 256 bytes as Rivet decodes and prints them (`i8080_tool dis`), assembled by `asl`: same bytes, so the decoder, the printer and every length are checked against a third-party assembler |
| `i8080-oracle-diff.sh` | seeded generated programs (`i8080_tool gen`) in the model, `altairz80` and `vm80a_sim`: every OUT and the 64 KiB image against `vm80a_sim`, console and memory below `FF00` against `altairz80`; planted model bugs (`--plant`) must be caught |
| `i8080-oracle-cycles.sh` | the T-states of every form, taken and not taken, in `vm80a_sim` and the model |

## Differences between the oracles

Not model bugs; the generated programs avoid them where they would stop a
comparison, and say so:

- `altairz80` executes the alias bytes `10 18 20 28 30 38` as Z80 relative jumps
  and `08` as `EX AF,AF'`; the K1 profile (and `vm80a_sim`) treat them as `NOP`.
  Programs that use alias bytes (seeds divisible by 3) are not run there.
- `altairz80` maps devices on ports `01-05 0a 10-19 28-2b 32 33 a0-a7 fd-ff` and
  has a boot ROM at `FF00`-`FFFF`; the programs use ports `60-6f` (in), `70-7f`
  and `80` (out) and the image is compared below `FF00`.
- `altairz80` does not stop `HLT` while interrupts are enabled; programs end
  with `DI`.
- `asl` accepts `ldax h` as `mov a,m`; Rivet refuses it (`oracle-known.txt`).

## The model (`rivet.compute`, `I8080_sem`)

`rivet.compute` is target neutral: fixed-width values and byte memory as two
signatures, a concrete interpretation (`int`, widths up to 30), a term-building
one (`Compute.Term`, with `eval`) and a few derived operations. Each entry of
the descriptor table (`I8080_ops`) carries its effect, written once against a
small `MACHINE` signature, and its T-states. `I8080_sem.Make` supplies the
machine (registers, five flags, stack, `IN`/`OUT` as an environment, `HLT`, `EI`
delayed by one instruction, interrupt-supplied instructions, an ordered access
list) over any value interpretation and compiles the table to one closure per
opcode byte. `Model` is the concrete instance and `I8080_run` runs a program
under the K5 shim. `8080EX1` takes about two minutes and counts the same
23,803,352,043 T-states and 2,919,049,315 instructions as `vm80a_sim`.

`test_symbolic` steps each of the 256 opcode bytes once in the concrete and the
term interpretation from the same state, with registers and flags free, and
checks that the expression evaluates to the concrete result, including every
memory and port access. A conditional branch on a free flag raises
`Compute.Term.Undecided`: a driver that explores paths would catch it there.
