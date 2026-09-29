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
	.file	"global_ldst.c"
	.text
	.align	2
	.global	asm_test_entry
	.syntax unified
	.arm
	.type	asm_test_entry, %function
asm_test_entry:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	movw	r3, #:lower16:asm_test_global
	movt	r3, #:upper16:asm_test_global
	ldr	r0, [r3]
	add	r0, r0, #22
	str	r0, [r3]
	bx	lr
	.size	asm_test_entry, .-asm_test_entry
	.global	asm_test_global
	.data
	.align	2
	.type	asm_test_global, %object
	.size	asm_test_global, 4
asm_test_global:
	.word	20
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
