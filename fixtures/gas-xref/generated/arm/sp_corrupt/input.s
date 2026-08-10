	.syntax unified
	.arch armv7-a
	.fpu vfpv3-d16
	.arm
	.text
	.globl asm_snippet
asm_snippet:
	sub sp, sp, #16
	mov r0, #42
	bx lr
