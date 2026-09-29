	.file	"args_arith.c"
	.text
	.globl	asm_test_sum
	.type	asm_test_sum, @function
asm_test_sum:
	movl	12(%esp), %eax
	imull	8(%esp), %eax
	addl	4(%esp), %eax
	subl	16(%esp), %eax
	ret
	.size	asm_test_sum, .-asm_test_sum
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	pushl	%ebx
	subl	$16, %esp
	movl	$7, 12(%esp)
	movl	$5, 8(%esp)
	movl	$9, 4(%esp)
	movl	12(%esp), %eax
	movl	8(%esp), %ebx
	movl	4(%esp), %ecx
	movl	8(%esp), %edx
	imull	%ebx, %eax
	subl	%ecx, %eax
	leal	6(%eax,%edx,2), %eax
	addl	$16, %esp
	popl	%ebx
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
