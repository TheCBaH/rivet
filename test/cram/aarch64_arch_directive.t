gcc opens every AArch64 file with `.arch armv8-a`, the base profile this target
implements. It is understood and changes nothing; any other level would widen
what may be assembled, so it is still an unknown directive.

  $ asm() { ../../tool/asm.exe "$@"; }

  $ printf '\t.arch armv8-a\n\t.text\nf:\n\tret\n' > base.s
  $ asm --target aarch64 base.s > /dev/null && echo assembles
  assembles

  $ printf '\t.arch armv8.2-a\n\t.text\nf:\n\tret\n' > wider.s
  $ asm --target aarch64 wider.s > /dev/null 2> err.txt; head -1 err.txt | sed -e 's|^[^ ]*:||'
   error[simplify.directive]: unknown directive .arch
