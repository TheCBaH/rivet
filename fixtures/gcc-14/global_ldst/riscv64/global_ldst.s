	.file	"global_ldst.c"
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
	lui	a5,%hi(asm_test_global)
	lw	a0,%lo(asm_test_global)(a5)
	addiw	a0,a0,22
	sw	a0,%lo(asm_test_global)(a5)
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.globl	asm_test_global
	.data
	.align	2
	.type	asm_test_global, @object
	.size	asm_test_global, 4
asm_test_global:
	.word	20
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
