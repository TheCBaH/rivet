	.syntax unified
	.arch armv7-a
	.fpu vfpv3-d16
	.arm
	.text
	.globl asm_snippet
asm_snippet:
	mov ip, sp
	sub sp, sp, #8
	str ip, [sp, #0]
	str lr, [sp, #4]
	mov r0, #42
	ldr lr, [sp, #4]
	add sp, sp, #8
	bx lr
