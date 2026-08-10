	.text
	.globl asm_snippet
asm_snippet:
	leaq -16377(%rsp), %rax
	movb $0, (%rax)
	movl $42, %eax
	ret
