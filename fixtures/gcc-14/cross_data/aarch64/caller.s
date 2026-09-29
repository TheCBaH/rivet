	.arch armv8-a
	.file	"caller.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	adrp	x1, shared_value
	ldr	w0, [x1, #:lo12:shared_value]
	add	w0, w0, 22
	str	w0, [x1, #:lo12:shared_value]
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
