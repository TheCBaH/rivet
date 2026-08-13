	.text
	.globl asm_snippet
asm_snippet:
	movq %rsp, %rax
	andq $15, %rax
	cmpq $8, %rax
	movl $42, %eax
	je (. + 7)
	movl $41, %eax
	ret
