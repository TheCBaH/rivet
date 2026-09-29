	.file	"args_arith.c"
	.text
	.globl	asm_test_sum
	.type	asm_test_sum, @function
asm_test_sum:
	imull	%edx, %esi
	leal	(%rsi,%rdi), %eax
	subl	%ecx, %eax
	ret
	.size	asm_test_sum, .-asm_test_sum
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$7, -4(%rsp)
	movl	$5, -8(%rsp)
	movl	$9, -12(%rsp)
	movl	-4(%rsp), %eax
	movl	-8(%rsp), %esi
	movl	-12(%rsp), %ecx
	movl	-8(%rsp), %edx
	imull	%esi, %eax
	subl	%ecx, %eax
	leal	6(%rax,%rdx,2), %eax
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
