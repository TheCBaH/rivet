(* Extracted VERBATIM from scripts/asm-fixture-exec.sh's startup_for by script,
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
      {|	.section .text.startup,"ax",@progbits
	.balign 16
	.globl _start
_start:
	andl	$-16, %esp
	call	asm_test_entry
	movl	%eax, %ebx
	movl	$252, %eax	/* exit_group */
	int	$0x80
|}
  | Target.X86_64 ->
      {|	.section .text.startup,"ax",@progbits
	.balign 16
	.globl _start
_start:
	andq	$-16, %rsp
	call	asm_test_entry
	movl	%eax, %edi
	movl	$231, %eax	/* exit_group */
	syscall
|}
  | Target.Arm ->
      {|	.syntax unified
	.arch armv7-a
	.arm
	.section .text.startup,"ax",%progbits
	.balign 4
	.globl _start
_start:
	bl	asm_test_entry
	mov	r7, #248	@ exit_group, r0 already holds the result
	svc	#0
|}
  | Target.Aarch64 ->
      {|	.section .text.startup,"ax",@progbits
	.balign 4
	.globl _start
_start:
	bl	asm_test_entry
	mov	x8, #94		// exit_group, x0 already holds the result
	svc	#0
|}
  | Target.Riscv32 ->
      {|	.option nopic
	.option norelax
	.section .text.startup,"ax",@progbits
	.balign 4
	.globl _start
_start:
	call asm_test_entry
	li a7, 94		# exit_group, a0 already holds the result
	ecall
|}
  | Target.Riscv64 ->
      {|	.option nopic
	.option norelax
	.section .text.startup,"ax",@progbits
	.balign 4
	.globl _start
_start:
	call asm_test_entry
	li a7, 94		# exit_group, a0 already holds the result
	ecall
|}
