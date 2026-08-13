	.syntax unified
	.arch armv7-a
	.fpu vfpv3-d16
	.arm
	.text
	.globl asm_snippet
asm_snippet:
	mov r0, sp
	and r0, r0, #7
	cmp r0, #0
	moveq r0, #42
	movne r0, #41
	bx lr
