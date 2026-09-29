	.file	"callee.c"
	.text
	.globl	asm_test_callee
	.type	asm_test_callee, @function
asm_test_callee:
	leal	(%rdi,%rdi), %eax
	ret
	.size	asm_test_callee, .-asm_test_callee
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
