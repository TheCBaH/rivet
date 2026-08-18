(* Extracted VERBATIM from tools/asm-tool-gate.sh's snippet_for and
   expected_status_for by script, not retyped. That script was retired in
   Phase 8; this is now the definition rather than a copy of one - the same discipline Startup
   follows, and for a sharper reason here.

   Each target exits with a DIFFERENT status, so a mixed-up binary - the wrong
   emulator, or a stale artifact from another target - fails rather than passing
   on a shared value. That property is carried entirely by these literals: a
   transcription slip in one immediate would turn the discriminator into a
   coincidence, and the gate would still print "ok" as long as the slip happened
   to match the emulator that ran it.

   There is no libc here, no crt startup and no dynamic loader. The syscall is
   issued directly, which is what makes the check a test of the EMULATOR rather
   than of a sysroot - and why it establishes E2 tool compatibility only.

   gate_snippet_tests asserts byte-equality against the shell functions for all
   six targets, so the extraction stays honest as long as the shell exists. *)

let source = function
  | Target.X86_32 ->
      "\t.text\n\
       \t.globl _start\n\
       _start:\n\
       \tmovl\t$252, %eax\t/* exit_group */\n\
       \tmovl\t$11, %ebx\n\
       \tint\t$0x80\n"
  | Target.X86_64 ->
      "\t.text\n\
       \t.globl _start\n\
       _start:\n\
       \tmovq\t$231, %rax\t/* exit_group */\n\
       \tmovq\t$12, %rdi\n\
       \tsyscall\n"
  | Target.Arm ->
      "\t.syntax unified\n\
       \t.arch armv7-a\n\
       \t.arm\n\
       \t.text\n\
       \t.globl _start\n\
       _start:\n\
       \tmov\tr7, #248\t@ exit_group\n\
       \tmov\tr0, #13\n\
       \tsvc\t#0\n"
  | Target.Aarch64 ->
      "\t.text\n\
       \t.globl _start\n\
       _start:\n\
       \tmov\tx8, #94\t\t// exit_group\n\
       \tmov\tx0, #14\n\
       \tsvc\t#0\n"
  | Target.Riscv32 -> "\t.text\n\t.globl _start\n_start:\n\tli a7, 94\n\tli a0, 15\n\tecall\n"
  | Target.Riscv64 -> "\t.text\n\t.globl _start\n_start:\n\tli a7, 94\n\tli a0, 16\n\tecall\n"

let expected_status = function
  | Target.X86_32 -> 11
  | Target.X86_64 -> 12
  | Target.Arm -> 13
  | Target.Aarch64 -> 14
  | Target.Riscv32 -> 15
  | Target.Riscv64 -> 16
