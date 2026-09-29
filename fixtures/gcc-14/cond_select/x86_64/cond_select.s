	.file	"cond_select.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$20, -4(%rsp)
	movl	$22, -8(%rsp)
	movl	-4(%rsp), %edx
	movl	-8(%rsp), %eax
	cmpl	%eax, %edx
	jge	.L2
	movl	-8(%rsp), %edx
.L3:
	movl	-4(%rsp), %ecx
	movl	-8(%rsp), %eax
	cmpl	%eax, %ecx
	jle	.L4
	movl	-4(%rsp), %ecx
.L5:
	leal	20(%rdx), %eax
	cmpl	%ecx, %edx
	movl	$0, %edx
	cmovne	%edx, %eax
	ret
.L2:
	movl	-4(%rsp), %edx
	jmp	.L3
.L4:
	movl	-8(%rsp), %ecx
	jmp	.L5
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
