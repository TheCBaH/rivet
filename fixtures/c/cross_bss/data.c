/* The other half of M3's cross-file .bss/.comm slice - see caller.c.

   Deliberately uninitialized: a tentative definition of a non-static
   global is a construct compilers route to `.comm` or `.bss` rather than to
   an ordinary `.data` label. The GNU linker folds every `.comm` declaration
   of this name into a single reservation in the canonical `.bss` output
   section, which is the allocation this fixture exists to carry through this
   project's own internal linker (lib/image/image.ml's strong/common/weak
   resolver) instead. */
int shared_value;
