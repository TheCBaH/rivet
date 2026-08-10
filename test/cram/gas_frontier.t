How far this assembler gets on assembly files it did not write.

Every other test in this project runs over inputs the project produced: four
CompCert fixtures and, since test/snippets, whatever the AST corpus emits. This
one runs over the rest of the assembly in the tree - our own ABI helpers, and
CompCert's runtime library, the largest body of real A32/A64/x86 assembly
available here - and records exactly where each one stops.

GNU as assembles all 47, which is what makes the comparison mean something: the
inputs are known-good, so a rejection here is a statement about this assembler's
coverage and not about the file.

A rejection is not a failure. asm/docs/contracts.md §2.3 freezes M1 at the 24
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

The four committed fixtures are positive controls. They are the M1 scope, so
anything but "assembles" here is a regression rather than a boundary.

  $ for t in x86_32 x86_64 arm aarch64; do
  >   printf '%-8s %s\n' "$t" "$(verdict $t $corpus/$t/fixture-asm_test_entry/input.s)"
  > done
  x86_32   assembles
  x86_64   assembles
  arm      assembles
  aarch64  assembles

Our own ABI helpers. GNU as assembles all four; this assembler reads none of
them, and not for a reason connected to instruction coverage.

  $ for t in x86_32 x86_64 arm aarch64; do
  >   printf '%-8s %s\n' "$t" "$(verdict $t $corpus/$t/helper-helper/input.s)"
  > done
  x86_32    error[lex]: unexpected character '<'
  x86_64    error[lex]: unexpected character '\194'
  arm       error[lex]: unexpected character '\194'
  aarch64   error[lex]: unexpected character '\194'

CompCert's runtime library, per target.

  $ for t in x86_32 x86_64 arm aarch64; do
  >   for d in $corpus/$t/runtime-*/; do
  >     [ -d "$d" ] || continue
  >     printf '%-8s %-12s %s\n' "$t" "$(basename $d | sed 's/^runtime-//')" "$(verdict $t $d/input.s)"
  >   done
  > done
  x86_32   i64_dtos      error[x86.operand]: unknown register %ah
  x86_32   i64_dtou      error[x86.operand]: cannot parse operand LC1
  x86_32   i64_sar       error[x86.operand]: cannot parse operand 1f
  x86_32   i64_sdiv      error[x86.operand]: cannot parse operand 1f
  x86_32   i64_shl       error[x86.operand]: cannot parse operand 1f
  x86_32   i64_shr       error[x86.operand]: cannot parse operand 1f
  x86_32   i64_smod      error[x86.operand]: cannot parse operand 1f
  x86_32   i64_smulh     error[x86.simplify]: unknown instruction pushl
  x86_32   i64_stod      error[x86.simplify]: unknown instruction fildll
  x86_32   i64_stof      error[x86.simplify]: unknown instruction fildll
  x86_32   i64_udiv      error[x86.operand]: cannot parse operand __compcert_i64_udivmod
  x86_32   i64_udivmod   error[x86.operand]: cannot parse operand 1f
  x86_32   i64_umod      error[x86.operand]: cannot parse operand __compcert_i64_udivmod
  x86_32   i64_umulh     error[x86.simplify]: unknown instruction pushl
  x86_32   i64_utod      error[x86.operand]: cannot parse operand 1f
  x86_32   i64_utof      error[x86.operand]: cannot parse operand 1f
  x86_32   vararg        error[x86.operand]: cannot parse operand 3(%eax
  x86_64   i64_dtou      error[parse]: unexpected token
  x86_64   i64_utod      error[x86.operand]: cannot parse operand 1f
  x86_64   i64_utof      error[x86.operand]: cannot parse operand 1f
  x86_64   vararg        error[x86.operand]: cannot parse operand 1f
  arm      i64_dtos      error[arm.operand]: unknown register d0
  arm      i64_dtou      error[arm.operand]: unknown register d0
  arm      i64_sar       error[arm.operand]: cannot parse operand 1f
  arm      i64_sdiv      error[arm.operand]: cannot parse operand {r4 r5 r6 r7 r8 r10 lr}
  arm      i64_shl       error[arm.simplify]: unknown instruction and
  arm      i64_shr       error[arm.simplify]: unknown instruction and
  arm      i64_smod      error[arm.operand]: cannot parse operand {r4 r5 r6 r7 r8 r10 lr}
  arm      i64_smulh     error[arm.operand]: cannot parse operand {r4 r5 r6 r7}
  arm      i64_stod      error[parse]: unexpected token
  arm      i64_stof      error[parse]: unexpected token
  arm      i64_udiv      error[arm.operand]: cannot parse operand {r4 r5 r6 r7 r8 lr}
  arm      i64_udivmod   error[arm.operand]: unknown register eq
  arm      i64_umod      error[arm.operand]: cannot parse operand {r4 r5 r6 r7 r8 lr}
  arm      i64_umulh     error[arm.operand]: cannot parse operand {r4 r5 r6 r7}
  arm      i64_utod      error[parse]: unexpected token
  arm      i64_utof      error[parse]: unexpected token
  arm      vararg        error[arm.operand]: unknown register d0
  aarch64  vararg        error[aarch64.operand]: cannot parse operand 1f

Where the frontier actually is, as counts. This is the number to watch: it is
what M2 moves, and prose cannot regress.

  $ { for t in x86_32 x86_64 arm aarch64; do
  >     for d in $corpus/$t/*/; do verdict $t $d/input.s; done
  >   done; } | sed 's/line [0-9]* col [0-9]*: //' | sort | uniq -c | sort -rn
       11  error[x86.operand]: cannot parse operand 1f
        5  error[parse]: unexpected token
        4 assembles
        3  error[lex]: unexpected character '\194'
        3  error[arm.operand]: unknown register d0
        2  error[x86.simplify]: unknown instruction pushl
        2  error[x86.simplify]: unknown instruction fildll
        2  error[x86.operand]: cannot parse operand __compcert_i64_udivmod
        2  error[arm.simplify]: unknown instruction and
        2  error[arm.operand]: cannot parse operand {r4 r5 r6 r7}
        2  error[arm.operand]: cannot parse operand {r4 r5 r6 r7 r8 r10 lr}
        2  error[arm.operand]: cannot parse operand {r4 r5 r6 r7 r8 lr}
        1  error[x86.operand]: unknown register %ah
        1  error[x86.operand]: cannot parse operand LC1
        1  error[x86.operand]: cannot parse operand 3(%eax
        1  error[lex]: unexpected character '<'
        1  error[arm.operand]: unknown register eq
        1  error[arm.operand]: cannot parse operand 1f
        1  error[aarch64.operand]: cannot parse operand 1f
