	.file	"direct_call.c"
	.text
	.globl	asm_test_callee
	.type	asm_test_callee, @function
asm_test_callee:
	leal	(%rdi,%rdi), %eax
	ret
	.size	asm_test_callee, .-asm_test_callee
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$20, %edi
	call	asm_test_callee
	addl	$2, %eax
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
