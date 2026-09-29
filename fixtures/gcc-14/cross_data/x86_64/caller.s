	.file	"caller.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	shared_value(%rip), %eax
	addl	$22, %eax
	movl	%eax, shared_value(%rip)
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
