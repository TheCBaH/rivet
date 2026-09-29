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
	.file	"loop.c"
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
	mov	r3, #6
	str	r3, [sp, #4]
	ldr	r3, [sp, #4]
	cmp	r3, #0
	ble	.L4
	mov	r3, #0
	mov	r0, r3
.L3:
	add	r0, r0, #7
	add	r3, r3, #1
	ldr	r2, [sp, #4]
	cmp	r2, r3
	bgt	.L3
.L1:
	add	sp, sp, #8
	@ sp needed
	bx	lr
.L4:
	mov	r0, #0
	b	.L1
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
