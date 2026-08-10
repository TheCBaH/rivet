	.text
	.globl asm_snippet
asm_snippet:
	leaq -16376(%rsp), %rax
	movb $0, (%rax)
	movl $42, %eax
	ret
