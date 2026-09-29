	.arch armv8-a
	.file	"global_ldst.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	adrp	x1, asm_test_global
	ldr	w0, [x1, #:lo12:asm_test_global]
	add	w0, w0, 22
	str	w0, [x1, #:lo12:asm_test_global]
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.global	asm_test_global
	.data
	.align	2
	.type	asm_test_global, %object
	.size	asm_test_global, 4
asm_test_global:
	.word	20
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
