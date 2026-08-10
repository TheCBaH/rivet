	.text
	.globl asm_snippet
asm_snippet:
	pop %rdx
	subq $16, %rsp
	movl $42, %eax
	jmp *%rdx
