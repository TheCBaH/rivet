	.file	"loop.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	subl	$16, %esp
	movl	$6, 12(%esp)
	movl	12(%esp), %eax
	testl	%eax, %eax
	jle	.L4
	movl	$0, %eax
	movl	$0, %edx
	.p2align 4
.L3:
	addl	$7, %edx
	addl	$1, %eax
	movl	12(%esp), %ecx
	cmpl	%eax, %ecx
	jg	.L3
.L1:
	movl	%edx, %eax
	addl	$16, %esp
	ret
.L4:
	movl	$0, %edx
	jmp	.L1
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
