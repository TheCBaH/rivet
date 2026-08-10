The GNU as cross-reference, through the CLI.

test/xref asserts the same equality through the library API. This transcript
exists for what that cannot show: the command a person actually runs, the
disassembly they actually see, and the exit status they actually get. Those are
the project's interface to anyone holding a .s file, and nothing else exercises
them over a corpus.

The comparison is on bytes and is exact. `--dump-disasm=diagnostic` prints
address, bytes, canonical spelling and form id, so extracting column two gives
what this assembler produced; the committed text.hex is what GNU as produced
from the same input. Comparing the *spellings* would need the normalizations
test/differential declares, and would establish something weaker.

  $ asm() { ../../tool/asm.exe "$@"; }
  $ corpus=../../fixtures/gas-xref

Column two of the diagnostic dump, flattened - our bytes in memory order, in
text.hex's own format so the two are directly comparable.

  $ ourbytes() {
  >   asm --target "$1" --dump-disasm=diagnostic --fixed-base 0x0 "$2" \
  >     | awk -F'  +' '{print $2}' | tr '\n' ' ' | tr -s ' ' | sed 's/ $//'
  > }
  $ gasbytes() { tr -s ' \n' ' ' < "$1" | sed 's/ $//'; }

  $ for d in $corpus/generated/*/*/; do
  >   t=$(basename $(dirname $d)); c=$(basename $d)
  >   ours=$(ourbytes $t $d/input.s); gnu=$(gasbytes $d/text.hex)
  >   if [ "$ours" = "$gnu" ]; then echo "$t/$c: agree"
  >   else echo "$t/$c: DIFFER"; echo "  ours $ours"; echo "  gnu  $gnu"; fi
  > done
  aarch64/callee_clobber: agree
  aarch64/return41: agree
  aarch64/return42: agree
  aarch64/sp_corrupt: agree
  aarch64/stack_full: agree
  aarch64/stack_over: agree
  aarch64/trap: agree
  arm/callee_clobber: agree
  arm/return41: agree
  arm/return42: agree
  arm/sp_corrupt: agree
  arm/stack_full: agree
  arm/stack_over: agree
  arm/trap: agree
  x86_32/callee_clobber: agree
  x86_32/return41: agree
  x86_32/return42: agree
  x86_32/sp_corrupt: agree
  x86_32/stack_full: agree
  x86_32/stack_over: agree
  x86_32/trap: agree
  x86_64/callee_clobber: agree
  x86_64/return41: agree
  x86_64/return42: agree
  x86_64/sp_corrupt: agree
  x86_64/stack_full: agree
  x86_64/stack_over: agree
  x86_64/trap: agree

One case in full, so the transcript carries a worked example rather than only a
verdict. AArch64 because it is the profile where the two disassemblers disagree
most about spelling and least about bytes: [add xD, sp, #0] is printed [mov],
and [ret x30] is printed [ret], by both.

  $ asm --target aarch64 --dump-disasm=diagnostic --fixed-base 0x0 \
  >   $corpus/generated/aarch64/return42/input.s
  00000000  ef 03 00 91  mov x15, sp                [aarch64.add-imm]
  00000004  ef 7b bf a9  stp x15, x30, [sp, #-16]!  [aarch64.stp-pre]
  00000008  40 05 80 52  movz w0, #42               [aarch64.movz]
  0000000c  fe 07 40 f9  ldr x30, [sp, #8]          [aarch64.ldr-uoff]
  00000010  ff 43 00 91  add sp, sp, #16            [aarch64.add-imm]
  00000014  c0 03 5f d6  ret                        [aarch64.ret]

  $ cat $corpus/generated/aarch64/return42/objdump.txt
  
  
  Disassembly of section .text:
  
  0000000000000000 <asm_snippet>:
     0:	910003ef 	mov	x15, sp
     4:	a9bf7bef 	stp	x15, x30, [sp, #-16]!
     8:	52800540 	mov	w0, #0x2a                  	// #42
     c:	f94007fe 	ldr	x30, [sp, #8]
    10:	910043ff 	add	sp, sp, #0x10
    14:	d65f03c0 	ret



The round trip target.ml promises of `--dump-disasm=canonical`: "exactly
re-parseable: feeding this back through the assembler must produce the identical
image, and a round-trip test does exactly that". No test did. test/differential
compares the canonical dump against objdump but never feeds it back, so this is
the first place the claim is actually exercised.

The dump is a disassembly of a byte *range*, so it carries instructions and
nothing else - no section, no symbol, because a byte range has none to report.
Re-assembling it therefore needs the same context any consumer supplies, which
is what the corpus emitter puts in front of the instructions in the first place.
Prepending it here is part of the round trip, not a workaround for it: what is
being checked is that the *instruction text* survives, and it does.

  $ for d in $corpus/generated/*/*/; do
  >   t=$(basename $(dirname $d)); c=$(basename $d)
  >   sed -n '1,/^asm_snippet:/p' $d/input.s > round.s
  >   asm --target $t --dump-disasm=canonical --fixed-base 0x0 $d/input.s >> round.s
  >   before=$(ourbytes $t $d/input.s); after=$(ourbytes $t round.s)
  >   if [ "$before" = "$after" ]; then echo "$t/$c: round trip identical"
  >   else echo "$t/$c: ROUND TRIP CHANGED THE IMAGE"; echo "  before $before"; echo "  after  $after"; fi
  > done
  aarch64/callee_clobber: round trip identical
  aarch64/return41: round trip identical
  aarch64/return42: round trip identical
  aarch64/sp_corrupt: round trip identical
  aarch64/stack_full: round trip identical
  aarch64/stack_over: round trip identical
  aarch64/trap: round trip identical
  arm/callee_clobber: round trip identical
  arm/return41: round trip identical
  arm/return42: round trip identical
  arm/sp_corrupt: round trip identical
  arm/stack_full: round trip identical
  arm/stack_over: round trip identical
  arm/trap: round trip identical
  x86_32/callee_clobber: round trip identical
  x86_32/return41: round trip identical
  x86_32/return42: round trip identical
  x86_32/sp_corrupt: round trip identical
  x86_32/stack_full: round trip identical
  x86_32/stack_over: round trip identical
  x86_32/trap: round trip identical
  x86_64/callee_clobber: round trip identical
  x86_64/return41: round trip identical
  x86_64/return42: round trip identical
  x86_64/sp_corrupt: round trip identical
  x86_64/stack_full: round trip identical
  x86_64/stack_over: round trip identical
  x86_64/trap: round trip identical
