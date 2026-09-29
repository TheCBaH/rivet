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
	.file	"args_arith.c"
	.text
	.align	2
	.global	asm_test_sum
	.syntax unified
	.arm
	.type	asm_test_sum, %function
asm_test_sum:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mla	r0, r2, r1, r0
	sub	r0, r0, r3
	bx	lr
	.size	asm_test_sum, .-asm_test_sum
	.align	2
	.global	asm_test_entry
	.syntax unified
	.arm
	.type	asm_test_entry, %function
asm_test_entry:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	sub	sp, sp, #16
	mov	r3, #7
	str	r3, [sp, #12]
	mov	r3, #5
	str	r3, [sp, #8]
	mov	r3, #9
	str	r3, [sp, #4]
	ldr	r0, [sp, #12]
	ldr	r1, [sp, #8]
	ldr	r2, [sp, #4]
	ldr	r3, [sp, #8]
	mul	r0, r1, r0
	sub	r0, r0, r2
	add	r0, r0, r3, lsl #1
	add	r0, r0, #6
	add	sp, sp, #16
	@ sp needed
	bx	lr
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
