	.file	"caller.c"
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
	sw	ra,12(sp)
	.cfi_offset 1, -4
	li	a0,20
	call	asm_test_callee
	addi	a0,a0,2
	lw	ra,12(sp)
	.cfi_restore 1
	addi	sp,sp,16
	.cfi_def_cfa_offset 0
	jr	ra
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
