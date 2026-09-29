	.arch armv8-a
	.file	"loop.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	w0, 6
	str	w0, [sp, 12]
	ldr	w0, [sp, 12]
	cmp	w0, 0
	ble	.L4
	mov	w1, 0
	mov	w0, 0
.L3:
	add	w0, w0, 7
	add	w1, w1, 1
	ldr	w2, [sp, 12]
	cmp	w2, w1
	bgt	.L3
.L1:
	add	sp, sp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	ret
.L4:
	.cfi_restore_state
	mov	w0, 0
	b	.L1
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
