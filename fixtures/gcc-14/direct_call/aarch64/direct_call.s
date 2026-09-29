	.arch armv8-a
	.file	"direct_call.c"
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
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB1:
	.cfi_startproc
	stp	x29, x30, [sp, -16]!
	.cfi_def_cfa_offset 16
	.cfi_offset 29, -16
	.cfi_offset 30, -8
	mov	x29, sp
	mov	w0, 20
	bl	asm_test_callee
	add	w0, w0, 2
	ldp	x29, x30, [sp], 16
	.cfi_restore 30
	.cfi_restore 29
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE1:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
