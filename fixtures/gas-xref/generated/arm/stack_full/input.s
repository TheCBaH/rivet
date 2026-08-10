	.syntax unified
	.arch armv7-a
	.fpu vfpv3-d16
	.arm
	.text
	.globl asm_snippet
asm_snippet:
	sub r1, sp, #16384
	mov r0, #0
	strb r0, [r1, #0]
	mov r0, #42
	bx lr
