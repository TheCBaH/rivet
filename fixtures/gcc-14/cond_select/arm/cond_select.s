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
	.file	"cond_select.c"
	.text
	.align	2
	.global	asm_test_entry
	.syntax unified
	.arm
	.type	asm_test_entry, %function
asm_test_entry:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	sub	sp, sp, #8
	mov	r3, #20
	str	r3, [sp, #4]
	mov	r3, #22
	str	r3, [sp]
	ldr	r2, [sp, #4]
	ldr	r3, [sp]
	cmp	r2, r3
	ldrlt	r0, [sp]
	ldrge	r0, [sp, #4]
	ldr	r2, [sp, #4]
	ldr	r3, [sp]
	cmp	r2, r3
	ldrgt	r3, [sp, #4]
	ldrle	r3, [sp]
	cmp	r0, r3
	addeq	r0, r0, #20
	movne	r0, #0
	add	sp, sp, #8
	@ sp needed
	bx	lr
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
