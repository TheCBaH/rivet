	.file	"caller.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	subl	$24, %esp
	pushl	$20
	call	asm_test_callee
	addl	$2, %eax
	addl	$28, %esp
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
