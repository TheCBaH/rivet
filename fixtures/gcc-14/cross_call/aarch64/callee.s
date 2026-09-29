	.arch armv8-a
	.file	"callee.c"
	.text
	.align	2
	.global	asm_test_callee
	.type	asm_test_callee, %function
asm_test_callee:
.LFB0:
	.cfi_startproc
	lsl	w0, w0, 1
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_callee, .-asm_test_callee
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
