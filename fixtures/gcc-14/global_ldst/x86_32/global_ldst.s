	.file	"global_ldst.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	asm_test_global, %eax
	addl	$22, %eax
	movl	%eax, asm_test_global
	ret
	.size	asm_test_entry, .-asm_test_entry
	.globl	asm_test_global
	.data
	.align 4
	.type	asm_test_global, @object
	.size	asm_test_global, 4
asm_test_global:
	.long	20
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
