	.arch armv8-a
	.file	"asm_test_entry.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	mov	w0, 42
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
