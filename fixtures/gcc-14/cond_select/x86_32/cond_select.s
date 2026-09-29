	.file	"cond_select.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	subl	$16, %esp
	movl	$20, 12(%esp)
	movl	$22, 8(%esp)
	movl	12(%esp), %edx
	movl	8(%esp), %eax
	cmpl	%eax, %edx
	jge	.L2
	movl	8(%esp), %edx
.L3:
	movl	12(%esp), %ecx
	movl	8(%esp), %eax
	cmpl	%eax, %ecx
	jle	.L4
	movl	12(%esp), %ecx
.L5:
	leal	20(%edx), %eax
	cmpl	%ecx, %edx
	movl	$0, %edx
	cmovne	%edx, %eax
	addl	$16, %esp
	ret
.L2:
	movl	12(%esp), %edx
	jmp	.L3
.L4:
	movl	8(%esp), %ecx
	jmp	.L5
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
