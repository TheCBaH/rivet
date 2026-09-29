	.arch armv8-a
	.file	"cond_select.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	w0, 20
	str	w0, [sp, 12]
	mov	w0, 22
	str	w0, [sp, 8]
	ldr	w1, [sp, 12]
	ldr	w0, [sp, 8]
	cmp	w1, w0
	bge	.L2
	ldr	w1, [sp, 8]
.L3:
	ldr	w2, [sp, 12]
	ldr	w0, [sp, 8]
	cmp	w2, w0
	ble	.L4
	ldr	w2, [sp, 12]
.L5:
	add	w0, w1, 20
	cmp	w1, w2
	csel	w0, w0, wzr, eq
	add	sp, sp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	ret
.L2:
	.cfi_restore_state
	ldr	w1, [sp, 12]
	b	.L3
.L4:
	ldr	w2, [sp, 8]
	b	.L5
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
