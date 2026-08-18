(* Extracted VERBATIM from tools/asm-fixture-exec.sh's startup_for by script,
   not retyped. That script was retired in Phase 8; this is now the definition
   rather than a copy of one. These are assembly sources whose bytes are assembled and then
   compared against a committed size, so a transcription slip would surface as
   a puzzling .start-size mismatch rather than as a visible typo.

   The byte comparison against that shell function retired with it. What covers
   these bytes now is behavioural and stronger: the stub calls asm_test_entry
   and exits with its result, so a wrong one makes the fixture exit with the
   wrong status under QEMU, which asm-exec fails on. *)

let source = function
  | Target.X86_32 ->
      "\t.section .text.startup,\"ax\",@progbits\n\
       \t.balign 16\n\
       \t.globl _start\n\
       _start:\n\
       \tandl\t$-16, %esp\n\
       \tcall\tasm_test_entry\n\
       \tmovl\t%eax, %ebx\n\
       \tmovl\t$252, %eax\t/* exit_group */\n\
       \tint\t$0x80\n"
  | Target.X86_64 ->
      "\t.section .text.startup,\"ax\",@progbits\n\
       \t.balign 16\n\
       \t.globl _start\n\
       _start:\n\
       \tandq\t$-16, %rsp\n\
       \tcall\tasm_test_entry\n\
       \tmovl\t%eax, %edi\n\
       \tmovl\t$231, %eax\t/* exit_group */\n\
       \tsyscall\n"
  | Target.Arm ->
      "\t.syntax unified\n\
       \t.arch armv7-a\n\
       \t.arm\n\
       \t.section .text.startup,\"ax\",%progbits\n\
       \t.balign 4\n\
       \t.globl _start\n\
       _start:\n\
       \tbl\tasm_test_entry\n\
       \tmov\tr7, #248\t@ exit_group, r0 already holds the result\n\
       \tsvc\t#0\n"
  | Target.Aarch64 ->
      "\t.section .text.startup,\"ax\",@progbits\n\
       \t.balign 4\n\
       \t.globl _start\n\
       _start:\n\
       \tbl\tasm_test_entry\n\
       \tmov\tx8, #94\t\t// exit_group, x0 already holds the result\n\
       \tsvc\t#0\n"
  | Target.Riscv32 ->
      "\t.option nopic\n\
       \t.option norelax\n\
       \t.section .text.startup,\"ax\",@progbits\n\
       \t.balign 4\n\
       \t.globl _start\n\
       _start:\n\
       \tcall asm_test_entry\n\
       \tli a7, 94\t\t# exit_group, a0 already holds the result\n\
       \tecall\n"
  | Target.Riscv64 ->
      "\t.option nopic\n\
       \t.option norelax\n\
       \t.section .text.startup,\"ax\",@progbits\n\
       \t.balign 4\n\
       \t.globl _start\n\
       _start:\n\
       \tcall asm_test_entry\n\
       \tli a7, 94\t\t# exit_group, a0 already holds the result\n\
       \tecall\n"
