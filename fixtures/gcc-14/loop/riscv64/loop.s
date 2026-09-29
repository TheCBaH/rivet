	.file	"loop.c"
	.option nopic
	.option norelax
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
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
	li	a5,6
	sw	a5,12(sp)
	lw	a5,12(sp)
	sext.w	a5,a5
	ble	a5,zero,.L4
	li	a4,0
	li	a0,0
.L3:
	addiw	a0,a0,7
	addiw	a4,a4,1
	lw	a5,12(sp)
	sext.w	a5,a5
	bgt	a5,a4,.L3
.L2:
	addi	sp,sp,16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	jr	ra
.L4:
	.cfi_restore_state
	li	a0,0
	j	.L2
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
