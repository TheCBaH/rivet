How far this assembler gets on assembly files it did not write.

Every other test in this project runs over inputs the project produced: six
the compiler fixtures and, since test/snippets, whatever the AST corpus emits. This
one runs over the rest of the assembly in the tree - our own ABI helpers, and
the compiler's runtime library, the largest body of real assembly available here -
across all six targets, and records exactly where each one stops.

GNU as assembles every one of them, which is what makes the comparison mean
something: the inputs are known-good, so a rejection here is a statement about
this assembler's coverage and not about the file.

A rejection is not a failure. docs/contracts.md §2.3 freezes M1 at the 24
encoding forms the fixtures emit and §4.3 lists what is deliberately absent, so
most of these files cannot assemble yet by construction. What this transcript
adds is that the boundary is now *measured* rather than described: every M2 form
moves a file, and a file that moves the wrong way shows up as a diff.

  $ asm() { ../../tool/asm.exe "$@"; }
  $ corpus=../../fixtures/gas-xref/frontier

The first diagnostic only. These files are thousands of lines long and the
second error is almost always a consequence of the first, so a full log would
bury the finding it exists to report.

  $ verdict() {
  >   if asm --target "$1" "$2" > /dev/null 2> err.txt; then echo "assembles"
  >   else head -1 err.txt | sed -e 's|^[^ ]*:||' -e 's/^\([0-9]*\):\([0-9]*\)/line \1 col \2/'; fi
  > }

The six committed fixtures are positive controls. They are the M1 scope, so
anything but "assembles" here is a regression rather than a boundary.

  $ for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
  >   printf '%-8s %s\n' "$t" "$(verdict $t $corpus/$t/fixture-asm_test_entry/input.s)"
  > done
  x86_32   assembles
  x86_64   assembles
  arm      assembles
  aarch64  assembles
  riscv32  assembles
  riscv64  assembles

Our own ABI helpers. GNU as assembles all six; this assembler reads none of
the four legacy ones, and not for a reason connected to instruction coverage -
gas-xref's own frontier_sources reads `helpers/<target>.s` unconditionally
and the legacy four are the *linked ELF's* own manifest/result-window
addresses spelled as raw bytes mid-file (an execution-ABI convention, not GAS
syntax), which the lexer chokes on immediately. riscv32/riscv64 have no such
convention - `helpers/riscv.c` is freestanding C compiled straight to an
object, never through a `.s` intermediate GAS would need to parse - so their
row here is instead a real, ordinary `.s` snapshot of that same source
(helpers/riscv32.s/riscv64.s's own header comment), the first
Helper-group row either RISC-V profile has ever had.

  $ for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
  >   printf '%-8s %s\n' "$t" "$(verdict $t $corpus/$t/helper-helper/input.s)"
  > done
  x86_32    error[lex]: unexpected character '<'
  x86_64    error[lex]: unexpected character '\194'
  arm       error[lex]: unexpected character '\194'
  aarch64   error[lex]: unexpected character '\194'
  riscv32  assembles
  riscv64  assembles

the compiler's runtime library, per target.

  $ for t in x86_32 x86_64 arm aarch64; do
  >   for d in $corpus/$t/runtime-*/; do
  >     [ -d "$d" ] || continue
  >     printf '%-8s %-12s %s\n' "$t" "$(basename $d | sed 's/^runtime-//')" "$(verdict $t $d/input.s)"
  >   done
  > done

Where the frontier actually is, as counts. This is the number to watch: it is
what M2 moves, and prose cannot regress.

  $ { for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
  >     for d in $corpus/$t/*/; do verdict $t $d/input.s; done
  >   done; } | sed 's/line [0-9]* col [0-9]*: //' | sort | uniq -c | sort -rn
        8 assembles
        3  error[lex]: unexpected character '\194'
        1  error[lex]: unexpected character '<'
