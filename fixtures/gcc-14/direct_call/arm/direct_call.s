	.arch armv7-a
	.fpu vfpv3-d16
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 1
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"direct_call.c"
	.text
	.align	2
	.global	asm_test_callee
	.syntax unified
	.arm
	.type	asm_test_callee, %function
asm_test_callee:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	lsl	r0, r0, #1
	bx	lr
	.size	asm_test_callee, .-asm_test_callee
	.align	2
	.global	asm_test_entry
	.syntax unified
	.arm
	.type	asm_test_entry, %function
asm_test_entry:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	push	{r4, lr}
	mov	r0, #20
	bl	asm_test_callee
	add	r0, r0, #2
	pop	{r4, pc}
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
