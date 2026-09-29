	.file	"args_arith.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_sum
	.type	asm_test_sum, @function
asm_test_sum:
.LFB0:
	.cfi_startproc
	mul	a1,a1,a2
	add	a0,a1,a0
	sub	a0,a0,a3
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_sum, .-asm_test_sum
	.align	2
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
.LFB1:
	.cfi_startproc
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	li	a5,7
	sw	a5,12(sp)
	li	a5,5
	sw	a5,8(sp)
	li	a5,9
	sw	a5,4(sp)
	lw	a0,12(sp)
	lw	a3,8(sp)
	lw	a4,4(sp)
	lw	a5,8(sp)
	mul	a0,a0,a3
	sub	a0,a0,a4
	slli	a5,a5,1
	add	a0,a0,a5
	addi	a0,a0,6
	addi	sp,sp,16
	.cfi_def_cfa_offset 0
	jr	ra
	.cfi_endproc
.LFE1:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
