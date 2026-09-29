	.file	"callee.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_callee
	.type	asm_test_callee, @function
asm_test_callee:
.LFB0:
	.cfi_startproc
	slli	a0,a0,1
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_callee, .-asm_test_callee
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
