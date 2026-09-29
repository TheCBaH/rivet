	.file	"args_arith.c"
	.option nopic
	.option norelax
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_sum
	.type	asm_test_sum, @function
asm_test_sum:
.LFB0:
	.cfi_startproc
	mulw	a1,a1,a2
	addw	a0,a1,a0
	subw	a0,a0,a3
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
	lw	a5,12(sp)
	lw	a3,8(sp)
	lw	a4,4(sp)
	lw	a0,8(sp)
	sext.w	a0,a0
	mulw	a5,a5,a3
	subw	a5,a5,a4
	slliw	a0,a0,1
	addw	a0,a0,a5
	addiw	a0,a0,6
	addi	sp,sp,16
	.cfi_def_cfa_offset 0
	jr	ra
	.cfi_endproc
.LFE1:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
