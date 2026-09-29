/* M2's global load/store slice (docs/design.md §12, Milestone 2).

   The initializer is non-zero on purpose. Compilers route an uninitialized or
   all-zero global to .comm or .bss, and common allocation and NOBITS storage
   are both M3. A non-zero initializer puts the object in .data as
   ordinary PROGBITS bytes, which is the second allocatable section M2 wants
   and nothing more.

   This is also the fixture where the four targets stop agreeing about how a
   global is addressed at all - PC-relative on x86-64, absolute on x86-32, a
   page/low-12 pair on AArch64, a movw/movt pair on ARM - so it is the one that
   forces per-target relocation kinds rather than a shared guess. */
int asm_test_global = 20;

int asm_test_entry(void)
{
  int v = asm_test_global;
  asm_test_global = v + 22;
  return asm_test_global;
}
