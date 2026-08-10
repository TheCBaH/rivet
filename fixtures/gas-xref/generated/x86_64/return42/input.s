	.text
	.globl asm_snippet
asm_snippet:
	subq $8, %rsp
	leaq 16(%rsp), %rax
	movq %rax, (%rsp)
	movl $42, %eax
	addq $8, %rsp
	ret
