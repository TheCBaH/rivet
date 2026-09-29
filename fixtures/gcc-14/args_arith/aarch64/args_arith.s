	.arch armv8-a
	.file	"args_arith.c"
	.text
	.align	2
	.global	asm_test_sum
	.type	asm_test_sum, %function
asm_test_sum:
.LFB0:
	.cfi_startproc
	madd	w0, w1, w2, w0
	sub	w0, w0, w3
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_sum, .-asm_test_sum
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB1:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	w0, 7
	str	w0, [sp, 12]
	mov	w0, 5
	str	w0, [sp, 8]
	mov	w0, 9
	str	w0, [sp, 4]
	ldr	w1, [sp, 12]
	ldr	w3, [sp, 8]
	ldr	w2, [sp, 4]
	ldr	w0, [sp, 8]
	mul	w1, w1, w3
	sub	w1, w1, w2
	add	w0, w1, w0, lsl 1
	add	w0, w0, 6
	add	sp, sp, 16
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE1:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
