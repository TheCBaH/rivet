(* Extracted VERBATIM from scripts/asm-tool-gate.sh's snippet_for and
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
      {|	.text
	.globl _start
_start:
	movl	$252, %eax	/* exit_group */
	movl	$11, %ebx
	int	$0x80
|}
  | Target.X86_64 ->
      {|	.text
	.globl _start
_start:
	movq	$231, %rax	/* exit_group */
	movq	$12, %rdi
	syscall
|}
  | Target.Arm ->
      {|	.syntax unified
	.arch armv7-a
	.arm
	.text
	.globl _start
_start:
	mov	r7, #248	@ exit_group
	mov	r0, #13
	svc	#0
|}
  | Target.Aarch64 ->
      {|	.text
	.globl _start
_start:
	mov	x8, #94		// exit_group
	mov	x0, #14
	svc	#0
|}
  | Target.Riscv32 -> {|	.text
	.globl _start
_start:
	li a7, 94
	li a0, 15
	ecall
|}
  | Target.Riscv64 -> {|	.text
	.globl _start
_start:
	li a7, 94
	li a0, 16
	ecall
|}

let expected_status = function
  | Target.X86_32 -> 11
  | Target.X86_64 -> 12
  | Target.Arm -> 13
  | Target.Aarch64 -> 14
  | Target.Riscv32 -> 15
  | Target.Riscv64 -> 16
