	.file	"loop.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$6, -4(%rsp)
	movl	-4(%rsp), %eax
	testl	%eax, %eax
	jle	.L4
	movl	$0, %eax
	movl	$0, %edx
	.p2align 4
.L3:
	addl	$7, %edx
	addl	$1, %eax
	movl	-4(%rsp), %ecx
	cmpl	%eax, %ecx
	jg	.L3
.L1:
	movl	%edx, %eax
	ret
.L4:
	movl	$0, %edx
	jmp	.L1
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
