How far this assembler gets on assembly files it did not write.

Every other test in this project runs over inputs the project produced: six
CompCert fixtures and, since test/snippets, whatever the AST corpus emits. This
one runs over the rest of the assembly in the tree - our own ABI helpers, and
CompCert's runtime library, the largest body of real A32/A64/x86 assembly
available here (plus, for RISC-V, the one fixture control gas-xref's own
frontier_sources already covers for riscv32/riscv64) - and records exactly
where each one stops.

GNU as assembles every one of them, which is what makes the comparison mean
something: the inputs are known-good, so a rejection here is a statement about
this assembler's coverage and not about the file.

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

The six committed fixtures are positive controls. They are the M1 scope, so
anything but "assembles" here is a regression rather than a boundary. RISC-V
joins only this control: gas-xref.ml's own frontier_sources deliberately
covers just Fixture for riscv32/riscv64 (asm/tools/lib/gas_xref_cmd.ml's own
comment - no asm/helpers/riscv32.s/riscv64.s exists yet, and CompCert's
runtime tree names its RISC-V leg riscV, not riscv32/riscv64), so there is no
Helper or Runtime row below for either.

  $ for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
  >   printf '%-8s %s\n' "$t" "$(verdict $t $corpus/$t/fixture-asm_test_entry/input.s)"
  > done
  x86_32   assembles
  x86_64   assembles
  arm      assembles
  aarch64  assembles
  riscv32  assembles
  riscv64  assembles

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
  x86_32   i64_dtos      error[x86.simplify]: unknown instruction fnstcw
  x86_32   i64_dtou      error[x86.simplify]: unknown instruction fnstsw
  x86_32   i64_sar       error[x86.simplify]: 8-bit operands are not in M1 scope
  x86_32   i64_sdiv     <synthesized by x86.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  x86_32   i64_shl       error[x86.simplify]: 8-bit operands are not in M1 scope
  x86_32   i64_shr       error[x86.simplify]: 8-bit operands are not in M1 scope
  x86_32   i64_smod     <synthesized by x86.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  x86_32   i64_smulh     error[x86.simplify]: cmov takes two register operands in M2
  x86_32   i64_stod     assembles
  x86_32   i64_stof     assembles
  x86_32   i64_udiv     <synthesized by x86.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  x86_32   i64_udivmod  assembles
  x86_32   i64_umod     <synthesized by x86.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  x86_32   i64_umulh    assembles
  x86_32   i64_utod      error[simplify.directive]: .p2align takes a power-of-two exponent and is not in M2 scope; use .balign
  x86_32   i64_utof      error[simplify.directive]: .p2align takes a power-of-two exponent and is not in M2 scope; use .balign
  x86_32   vararg       assembles
  x86_64   i64_dtou      error[x86.simplify]: unknown instruction ucomisd
  x86_64   i64_utod      error[x86.simplify]: unknown instruction pxor
  x86_64   i64_utof      error[x86.simplify]: unknown instruction pxor
  x86_64   vararg        error[x86.simplify]: 8-bit operands are not in M1 scope
  arm      i64_dtos     assembles
  arm      i64_dtou     assembles
  arm      i64_sar      assembles
  arm      i64_sdiv     <synthesized by arm.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  arm      i64_shl      assembles
  arm      i64_shr      assembles
  arm      i64_smod     <synthesized by arm.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  arm      i64_smulh     error[arm.simplify]: unknown instruction umull
  arm      i64_stod      error[arm.simplify]: unknown instruction vmla.f64
  arm      i64_stof      error[arm.simplify]: unknown instruction vmla.f64
  arm      i64_udiv     <synthesized by arm.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  arm      i64_udivmod   error[arm.simplify]: unknown instruction it
  arm      i64_umod     <synthesized by arm.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
  arm      i64_umulh     error[arm.simplify]: unknown instruction umull
  arm      i64_utod      error[arm.simplify]: unknown instruction vmla.f64
  arm      i64_utof      error[arm.simplify]: unknown instruction lsrs
  arm      vararg       assembles
  aarch64  vararg       assembles

Where the frontier actually is, as counts. This is the number to watch: it is
what M2 moves, and prose cannot regress.

  $ { for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
  >     for d in $corpus/$t/*/; do verdict $t $d/input.s; done
  >   done; } | sed 's/line [0-9]* col [0-9]*: //' | sort | uniq -c | sort -rn
       18 assembles
        4 <synthesized by x86.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
        4 <synthesized by arm.encode>: error[image.undefined]: fixup target references undefined symbol __compcert_i64_udivmod
        4  error[x86.simplify]: 8-bit operands are not in M1 scope
        3  error[lex]: unexpected character '\194'
        3  error[arm.simplify]: unknown instruction vmla.f64
        2  error[x86.simplify]: unknown instruction pxor
        2  error[simplify.directive]: .p2align takes a power-of-two exponent and is not in M2 scope; use .balign
        2  error[arm.simplify]: unknown instruction umull
        1  error[x86.simplify]: unknown instruction ucomisd
        1  error[x86.simplify]: unknown instruction fnstsw
        1  error[x86.simplify]: unknown instruction fnstcw
        1  error[x86.simplify]: cmov takes two register operands in M2
        1  error[lex]: unexpected character '<'
        1  error[arm.simplify]: unknown instruction lsrs
        1  error[arm.simplify]: unknown instruction it
