	.file	"data.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.globl	shared_value
	.bss
	.align	2
	.type	shared_value, @object
	.size	shared_value, 4
shared_value:
	.zero	4
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
