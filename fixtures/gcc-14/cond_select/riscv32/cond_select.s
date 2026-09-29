	.file	"cond_select.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
.LFB0:
	.cfi_startproc
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	li	a5,20
	sw	a5,12(sp)
	li	a5,22
	sw	a5,8(sp)
	lw	a4,12(sp)
	lw	a5,8(sp)
	bge	a4,a5,.L2
	lw	a5,8(sp)
.L3:
	lw	a3,12(sp)
	lw	a4,8(sp)
	ble	a3,a4,.L4
	lw	a4,12(sp)
.L5:
	li	a0,0
	beq	a5,a4,.L9
.L1:
	addi	sp,sp,16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	jr	ra
.L2:
	.cfi_restore_state
	lw	a5,12(sp)
	j	.L3
.L4:
	lw	a4,8(sp)
	j	.L5
.L9:
	addi	a0,a5,20
	j	.L1
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
