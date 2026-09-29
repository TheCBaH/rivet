	.file	"caller.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	subq	$8, %rsp
	movl	$20, %edi
	call	asm_test_callee
	addl	$2, %eax
	addq	$8, %rsp
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
